-module(basin_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

init_per_testcase(_, Config) ->
	basin:start(),
	Config.

end_per_testcase(_, Config) ->
	ok = basin:stop(),
	Config.

all() -> [generate_upto_20, generate_upto_20_to_file, generate_same_primes, generate_and_kill_nodes, generate_and_kill_srvs].

generate_upto_20(_Config) ->
	Pid = basin:start_generate(20),
	link(Pid),
	ct:pal(debug, "generation started: ~p", [Pid]),
	Primes = basin:wait_until_done(Pid),
	ct:pal(debug, "generation done: ~p", [Primes]),
	Primes = primes_upto_20().

generate_upto_20_to_file(_Config) ->
	File = "primes.1.0",
	Pid = basin:start_generate(20),
	basin:wait_until_done_and_save_to_file(Pid, File),
	ExpPrimes = primes_upto_20(),
	{ok, [ExpPrimes]} = file:consult(File).

generate_same_primes(_Config) ->
	UpTo = 1000000,
	Pid1 = basin:start_generate(UpTo),
	Pid2 = basin:start_generate(UpTo),
	Primes1 = basin:wait_until_done(Pid1),
	Primes2 = basin:wait_until_done(Pid2),
	Primes1 = Primes2.

generate_and_kill_srvs(_Config) ->
	UpTo = 1000000,
	Pid1 = basin:start_generate(UpTo),
	Primes1 = basin:wait_until_done(Pid1),
	ct:pal(debug, "done generate etalon"),
	random:seed(now()),
	Srvs = get_workers(),
	Pid2 = basin:start_generate(UpTo),
	kill_some_workers(length(Srvs), Srvs),
	Primes2 = basin:wait_until_done(Pid2),
	Primes1 = Primes2.


kill_some_workers(0, _) -> ok;
kill_some_workers(Count, Srvs) ->
	Index = random:uniform(length(Srvs)),
	{Name, Node} = lists:nth(Index, Srvs),
	ct:pal(debug, "terminate srv ~p on node ~p", [Name, Node]),
	supervisor:terminate_child({basin_primes_sup, Node}, Name),
	timer:sleep(100),
	supervisor:restart_child({basin_primes_sup, Node}, Name),
	timer:sleep(100),
	kill_some_workers(Count - 1, Srvs).


generate_and_kill_nodes(_Config) ->
	UpTo = 10000000,
	Nodes = 10,
	run_nodes(Nodes),
	Pid1 = basin:start_generate(UpTo),
	Primes1 = basin:wait_until_done(Pid1),
	ct:pal(debug, "done generate etalon"),
	Pid2 = basin:start_generate(UpTo),
	kill_nodes(Nodes),
	Primes2 = basin:wait_until_done(Pid2),
	Primes1 = Primes2.


kill_nodes(0) ->
	ok;
kill_nodes(Count) ->
	timer:sleep(200),
	Host = list_to_atom(net_adm:localhost()),
	Node = list_to_atom("test" ++ integer_to_list(Count)),
	ct:pal(debug, "kill node ~p@~p", [Node, Host]),
	ct_slave:stop(Host, Node),
	timer:sleep(200),
	kill_nodes(Count - 1).

run_nodes(0) ->
	ok;
run_nodes(Count) ->
	Host = list_to_atom(net_adm:localhost()),
	{ok, _} = ct_slave:start(Host, list_to_atom("test" ++ integer_to_list(Count)), [{monitor_master, true}, {erl_flags, " -pa ../../ebin/ -run basin start "}]),
	run_nodes(Count - 1).

primes_upto_20() ->
	[2, 3, 5, 7, 11, 13, 17, 19].

get_workers() ->
	catch net_adm:world(),
	{Wrkrs, _BadNodes} = rpc:multicall(basin_primes_sup, get_srvs, []),
	lists:flatten(filter_bad_rpc(Wrkrs, [])).

filter_bad_rpc([], Acc) ->
	Acc;
filter_bad_rpc([{badrpc, _} | Tail], Acc) ->
	filter_bad_rpc(Tail, Acc);
filter_bad_rpc([Head | Tail], Acc) ->
	filter_bad_rpc(Tail, [Head | Acc]).
