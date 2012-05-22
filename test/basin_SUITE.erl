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
	Primes = basin:generate(20),
	Primes = primes_upto_20().

generate_upto_20_to_file(_Config) ->
	Pid = basin:generate_to_file(20, "primes_20"),
	basin:wait_unitl_done(Pid),
	ExpPrimes = primes_upto_20(),
	{ok, [ExpPrimes]} = file:consult("primes_20"),
	file:delete("primes_20").

generate_same_primes(_Config) ->
	UpTo = 1000000,
	File1 = "primes.1.1",
	File2 = "primes.2.1",
	Pid = basin:generate_to_file(UpTo, File1),
	Pid1 = basin:generate_to_file(UpTo, File2),
	basin:wait_unitl_done(Pid),
	basin:wait_unitl_done(Pid1),
	{ok, [Primes1]} = file:consult(File1),
	{ok, [Primes2]} = file:consult(File2),
	Primes1 = Primes2.

generate_and_kill_srvs(_Config) ->
	UpTo = 1000000,
	File1 = "primes.1.2",
	File2 = "primes.2.2",
	Pid = basin:generate_to_file(UpTo, File1),
	basin:wait_unitl_done(Pid),
	ct:pal(debug, "done generate etalon"),
	random:seed(now()),
	Srvs = basin:get_srvs(),
	Pid1 = basin:generate_to_file(UpTo, File2),
	kill_some_workers(length(Srvs), Srvs),
	basin:wait_unitl_done(Pid1),
	{ok, [Primes1]} = file:consult(File1),
	{ok, [Primes2]} = file:consult(File2),
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
	UpTo = 4000000,
	File1 = "primes.1.3",
	File2 = "primes.2.3",
	Nodes = 4,
	run_nodes(Nodes),
	ct:pal(debug, "nodes ~p", [[node() | nodes()]]),
	ct:pal(debug, "workers ~p", [basin:get_srvs()]),
	Pid = basin:generate_to_file(UpTo, File1),
	basin:wait_unitl_done(Pid),
	ct:pal(debug, "done generate etalon"),
	Pid1 = basin:generate_to_file(UpTo, File2),
	%kill_nodes(Nodes),
	basin:wait_unitl_done(Pid1),
	{ok, [Primes1]} = file:consult(File1),
	{ok, [Primes2]} = file:consult(File2),
	Primes1 = Primes2.


kill_nodes(0) ->
	ok;
kill_nodes(Count) ->
	timer:sleep(200),
	Host = list_to_atom(net_adm:localhost()),
	Node = list_to_atom("test" ++ integer_to_list(Count)),
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
