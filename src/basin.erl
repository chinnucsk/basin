-module(basin).

-export([start/0, stop/0, generate_to_file/2, generate_progress/1, generate/1, wait_unitl_done/1, get_srvs/0]).

start() ->
	application:start(basin).

stop() ->
	application:stop(basin).

generate_to_file(Max, Filename) when is_integer(Max), Max > 0 ->
	FSPid = spawn_link(fun() -> receive_to_file(Filename) end),
	generate(Max, FSPid).

generate_progress(Pid) ->
	Pid ! {self(), get_progress},
	receive
		Some -> Some
	after 500 ->
			{notfound, Pid}
	end.

wait_unitl_done(Pid) ->
	case generate_progress(Pid) of
		{notfound, Pid} -> ok;
		{progress, Progress} ->
			io:format("progress: ~p - ~p~n", [Pid, Progress]),
			timer:sleep(1000),
			wait_unitl_done(Pid)
	end.

receive_to_file(Filename) ->
	receive
		{_Pid, result, Primes} ->
			{ok, FH} = file:open(Filename, [write]),
			io:fwrite(FH, "~p.~n", [Primes]),
			file:close(FH)
	end.

generate(Max) ->
	Self = self(),
	generate(Max, Self),
	receive
		{_Pid, result, Primes} ->
			Primes
	after 10000 ->
		throw(timeout)
	end.


generate(Max, ReturnTo) when is_integer(Max), Max > 0 ->
	spawn_link(fun() -> generator(Max, ReturnTo) end).

generator(Max, ReturnTo) ->
	Step = step(),
	process_flag(trap_exit, true),
	random:seed(now()),
	Srvs1 = get_srvs(),
	Srvs = case length(Srvs1) * Step > Max of
		false -> Srvs1;
		true -> lists:sublist(Srvs1, (Max div Step) + 1)
	end,
	lists:foreach(fun(Srv) -> monitor(process, Srv) end, Srvs),
	CurrentTasks = [run_test(I*Step, min((I + 1)*Step, Max), lists:nth(I+1, Srvs)) || I <- lists:seq(0, length(Srvs) - 1)],
	Result = generate(get_srvs(), CurrentTasks, Max, length(Srvs) * Step + 1, []),
	ReturnTo ! {self(), result, lists:usort(lists:flatten(Result))}.

generate(_Srvs, [], Max, From, Res) when From > Max ->
	Res;

generate(Srvs, TaskInfo, Max, From, Res) ->
	receive
		{Srv, test_result, Primes} ->
			Res1 = [Primes | Res],
			{NewFrom, NewTaskInfo} = run_test(From, Max, Srv, TaskInfo),
			generate(Srvs, NewTaskInfo, Max, NewFrom, Res1);
		{'DOWN', _Ref, _Type, Srv, noproc} ->
			{_Name, Node} = Srv,
			case is_connected(Node) of
				true -> monitor(process, Srv);
				_ -> false
			end,
			generate(Srvs, TaskInfo, Max, From, Res);
		{'DOWN', _Ref, _Type, Srv, _Info} ->
			ct:pal(debug, "Srv ~p down with reason ~p", [Srv, _Info]),
			case lists:keyfind(Srv, 1, TaskInfo) of
				false -> generate(Srvs, TaskInfo, Max, From, Res);
				{Srv, {SrvFrom, SrvTo}} ->
						TaskInfo1 = lists:keydelete(Srv, 1, TaskInfo),
						{Srvs1, NewTaskInfo} = run_test_in_new(SrvFrom, SrvTo, Srvs, TaskInfo1),
						generate(Srvs1, NewTaskInfo, Max, From, Res)
			end;
		{Pid, get_progress} ->
			Pid ! {progress, (From - length(TaskInfo) * step()) * 100 / Max},
			generate(Srvs, TaskInfo, Max, From, Res)
	end.


run_test(From, Max, Srv, TaskInfo) when From > Max ->
	{From, lists:keydelete(Srv, 1, TaskInfo)};

run_test(From, Max, Srv, TaskInfo) ->
	To = min(Max, From + step()),
	{To + 1, lists:keystore(Srv, 1, TaskInfo, run_test(From, To, Srv))}.

run_test_in_new(From, To, Srvs) ->
	Index = random:uniform(length(Srvs)),
	Srv = lists:nth(Index, Srvs),
	try
		Result = run_test(From, To, Srv),
		monitor(process, Srv),
		{Srvs, Result}
	catch
		_:_ ->
			Srvs1 = get_srvs(),
			run_test_in_new(From, To, Srvs1)
	end.

run_test_in_new(From, To, Srvs, Tasks) ->
	{Srvs1, {Srv, Range}} = run_test_in_new(From, To, Srvs),
	NewTasks = lists:keystore(Srv, 1, Tasks, {Srv, Range}),
	{Srvs1, NewTasks}.

run_test(From, To, Name) ->
	basin_primes_srv:test_range(From, To, Name, self()),
	{Name, {From, To}}.

get_srvs() ->
	catch net_adm:world(),
	{Srvs, _BadNodes} = rpc:multicall(basin_primes_sup, srvs, []),
	lists:flatten(Srvs).

is_connected(ENode) ->
    [N||N<-nodes(), N==ENode] == [ENode].

step() ->
	10000.
