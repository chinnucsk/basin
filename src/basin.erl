-module(basin).

-export([start/0, stop/0, generate_to_file/2, generate_progress/1]).

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

receive_to_file(Filename) ->
	receive
		{_Pid, result, Primes} ->
			{ok, FH} = file:open(Filename, [write]),
			io:fwrite(FH, "~p~n", [Primes]),
			file:close(FH)
	end.

generate(Max, ReturnTo) when is_integer(Max), Max > 0 ->
	spawn_link(fun() -> generator(Max, ReturnTo) end).


generator(Max, ReturnTo) ->
	process_flag(trap_exit, true),
	random:seed(now()),
	{Srvs, TaskSrvs} = basin_workers:load_workers(Max),
	TasksInfo = [basin_workers:run_worker(Task) || Task <- TaskSrvs],
	io:format("~p~n", [TasksInfo]),
	Result = collect_results(Srvs, TasksInfo, []),
	ReturnTo ! {self(), result, lists:usort(lists:flatten(Result))}.

collect_results(_Srvs, [], Results) ->
	Results;

collect_results(Srvs, TaskInfo, Results) ->
	receive
		{Pid, test_results, Result} ->
			Results1 = [Result | Results],
			TaskInfo1 = lists:keydelete(Pid, 1, TaskInfo),
			collect_results(Srvs, TaskInfo1, Results1);
		{'EXIT', _Pid, normal} ->
			collect_results(Srvs, TaskInfo, Results);
		{'EXIT', Pid, _Reason} ->
			{Pid, {Range, _Worker}} = lists:keyfind(Pid, 1, TaskInfo),
			NSrvIndex = random:uniform(length(Srvs)),
			NewTaskInfo = basin_workers:run_worker({Range, lists:nth(NSrvIndex, Srvs)}),
			{Pid1, _Task} = NewTaskInfo,
			TaskInfo1 = lists:keystore(Pid1, 1, lists:keydelete(Pid, 1, TaskInfo), NewTaskInfo),
			collect_results(Srvs, TaskInfo1, Results);
		{Pid, get_progress} ->
			Pid ! {progress, (length(Srvs) - length(TaskInfo)) * 100 / length(Srvs)},
			collect_results(Srvs, TaskInfo, Results)
	end.

