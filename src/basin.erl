-module(basin).

-export([start/0, stop/0]).

-export([start_generate/1, stop_generator/1, wait_until_done_and_save_to_file/2, get_progress/1, wait_until_done/1]).

start() ->
	application:start(basin).

stop() ->
	application:stop(basin).

-record(state, {monitor_ref = undefined, primes = [], done = false, reason = normal}).

start_generate(Max) when is_integer(Max), Max > 0 ->
	LPid = spawn(fun() -> generate_loop(undefined, #state{}) end),
	{ok, Pid} = supervisor:start_child(basin_gen_sup, [Max, LPid]),
	LPid ! {generator, Pid},
	LPid.

stop_generator(Pid) ->
	Pid ! stop,
	ok.


get_progress(Pid) ->
	Pid ! {self(), get_progress},
	receive
		{Pid, Progress, Value} ->
			{Progress, Value};
		Unknown ->
			exit({unknown_msg, Unknown})
	after 10000 ->
			exit(get_progress_timeout)
	end.

get_primes(Pid) ->
	Pid ! {self(), primes},
	receive
		{Pid, primes, Primes} -> Primes;
	Unknown ->
			exit({unknown_msg, Unknown})
	after 1000 ->
			exit(get_progress_timeout)
	end.

wait_until_done(Pid) ->
	case get_progress(Pid) of
		{generation_done, normal} ->
			Primes = get_primes(Pid),
			stop_generator(Pid),
			Primes;
		{generation_done, Reason} ->
			stop_generator(Pid),
			exit({generation_fail, Reason});
		{progress, P} ->
			io:format("generation ~p progress: ~p", [Pid, P]),
			timer:sleep(1000),
			wait_until_done(Pid)
	end.

wait_until_done_and_save_to_file(Pid, File) when is_list(File) ->
	case get_progress(Pid) of
		{generation_done, normal} ->
			Primes = get_primes(Pid),
			stop_generator(Pid),
			{ok, FH} = file:open(File, [write]),
			io:fwrite(FH, "~p.~n", [Primes]),
			file:close(FH),
			Primes;
		{generation_done, Reason} ->
			stop_generator(Pid),
			exit({generation_fail, Reason});
		{progress, P} ->
			io:format("generation ~p progress: ~p", [Pid, P]),
			timer:sleep(1000),
			wait_until_done_and_save_to_file(Pid, File)
	end.



generate_loop(undefined, State) ->
	receive
		{generator, Pid} ->
			generate_loop(Pid, State);
		Some ->
			exit({unknown_msg, Some})
	end;

generate_loop(Pid, State) when State#state.monitor_ref =:= undefined ->
	generate_loop(Pid, State#state{monitor_ref = monitor(process, Pid)});

generate_loop(Pid, State) ->
	receive
		{Pid, primes, Primes} ->
			generate_loop(Pid, State#state{primes = Primes});
		{'DOWN', _Ref, process, Pid, Reason} ->
			generate_loop(Pid, State#state{done = true, reason = Reason});
		{ReturnTo, get_progress} ->
			case State#state.done of
				true ->	ReturnTo ! {self(), generation_done, State#state.reason};
				_ -> ReturnTo ! {self(), progress, basin_gen_srv:get_progress(Pid)}
			end,
			generate_loop(Pid, State);
		{ReturnTo, primes} ->
			ReturnTo ! {self(), primes, State#state.primes};
		stop ->
			exit(normal);
		_Unknown ->
			exit({unknown_msg, _Unknown})
	end.

