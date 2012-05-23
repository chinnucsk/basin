-module(basin_gen_srv).

-behaviour(gen_server).

-export([start_link/2, get_progress/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {max, return_to, workers = [], primes = [], tasks = [], tasks_count = 0}).

get_progress(Pid) ->
	gen_server:call(Pid, get_progress).

start_link(Max, ReturnTo) ->
	gen_server:start_link(?MODULE, {Max, ReturnTo}, []).

init({Max, ReturnTo}) ->
	{ok, #state{max = Max, return_to = ReturnTo}, 0}.

handle_call(get_progress, _From, State) ->
    Reply = (State#state.tasks_count - length(State#state.tasks)) * 100 / State#state.tasks_count,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) when State#state.tasks =:= [], State#state.tasks_count =/= 0 ->
	case State#state.workers of
		[] ->
			State#state.return_to ! {self(), primes, lists:usort(lists:flatten(State#state.primes))},
			{stop, normal, State};
		_ ->
			{noreply, State}
	end;
handle_info(timeout, State) -> %% init work
	random:seed(now()),
	Wrks1 = get_workers(),
	case Wrks1 of
		[] -> {stop, no_workers_found, State};
		_ ->
			Tasks = split_on_tasks(0, State#state.max, []),
			{CW, Tasks1} = run_tasks(Tasks, Wrks1, []),
			{noreply, State#state{tasks = Tasks1, workers = CW, tasks_count = length(Tasks)}}
	end;

handle_info({Wrk, test_result, Primes}, State) when State#state.tasks =:= [] ->
	{noreply, State#state{primes = [Primes | State#state.primes], workers = lists:keydelete(Wrk, 1, State#state.workers)}, 0};

handle_info({Wrk, test_result, Primes}, State) ->
	NewPrimes = [Primes | State#state.primes],
	[{From, To} | Tasks1] = State#state.tasks,
	run_test(From, To, Wrk),
	{noreply, State#state{tasks = Tasks1, workers = lists:keystore(Wrk, 1, State#state.workers, {Wrk, {From, To}}), primes = NewPrimes}};
handle_info({'DOWN', _Ref, process, Wrk, _Info}, State) ->
	case lists:keyfind(Wrk, 1, State#state.workers) of
		false ->
			{noreply, State};
		{Wrk, {From, To}} ->
			NewWorkers1 = lists:keydelete(Wrk, 1, State#state.workers),
			Tasks = [{From, To} | State#state.tasks],
			Wrks = sets:from_list(get_workers()),
			CurWrks = sets:from_list([X || {X, _Range} <- State#state.workers]),
			Wrks1 = sets:to_list(sets:subtract(Wrks, CurWrks)),
			{CW, Tasks1} = run_tasks(Tasks, Wrks1, []),
			{noreply, State#state{workers = NewWorkers1 ++ CW, tasks = Tasks1}}
	end.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


get_workers() ->
	catch net_adm:world(),
	{Wrkrs, _BadNodes} = rpc:multicall(basin_primes_sup, srvs, []),
	lists:flatten(filter_bad_rpc(Wrkrs, [])).

filter_bad_rpc([], Acc) ->
	Acc;
filter_bad_rpc([{badrpc, _} | Tail], Acc) ->
	filter_bad_rpc(Tail, Acc);
filter_bad_rpc([Head | Tail], Acc) ->
	filter_bad_rpc(Tail, [Head | Acc]).

run_test(From, To, Name) ->
	basin_primes_srv:test_range(From, To, Name, self()),
	{Name, {From, To}}.

step() ->
	10000.

split_on_tasks(From, Max, Tasks) when From > Max ->
	Tasks;
split_on_tasks(From, Max, Tasks) ->
	Tasks1 = [{From, min(Max, From + step())} | Tasks],
	split_on_tasks(From + step() + 1, Max, Tasks1).

run_tasks([], _, Workers) ->
	{Workers, []};
run_tasks(Tasks, [],  Workers) ->
	{Workers, Tasks};
run_tasks([{From, To} | TTail], [WHead | WTail], Workers) ->
	run_test(From, To, WHead),
	monitor(process, WHead),
	run_tasks(TTail, WTail, [{WHead, {From, To}} | Workers]).
