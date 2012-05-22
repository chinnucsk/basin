-module(basin_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

node_name(NodeName) ->
	list_to_atom(atom_to_list(NodeName) ++ "@" ++ net_adm:localhost()).

init_per_suite(Config) ->
	basin:start(),
	%Host = list_to_atom(net_adm:localhost()),
	%{ok, _} = ct_slave:start(Host, 'test0@arzh', [{monitor_master, true}]),
	ct:pal(debug, "~p~n", [node() | nodes()]),
	Config.



end_per_suite(Config) ->
	basin:stop(),
	Config.

all() -> [generate_upto_20, generate_upto_20_to_file, generate_same_primes, generate_and_kill_srvs].

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
	File1 = "primes.1",
	File2 = "primes.2",
	Pid = basin:generate_to_file(UpTo, File1),
	Pid1 = basin:generate_to_file(UpTo, File2),
	basin:wait_unitl_done(Pid),
	basin:wait_unitl_done(Pid1),
	{ok, [Primes1]} = file:consult(File1),
	{ok, [Primes2]} = file:consult(File2),
	Primes1 = Primes2.

generate_and_kill_srvs(_Config) ->
	UpTo = 1000000,
	File1 = "primes.1",
	File2 = "primes.2",
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


%kill_some_workers(_, _) -> ok.

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

primes_upto_20() ->
	[2, 3, 5, 7, 11, 13, 17, 19].
