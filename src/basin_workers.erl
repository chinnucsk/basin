-module(basin_workers).

-export([load_workers/1, run_worker/1]).

-ifdef(TEST).
-compile(export_all).
-endif.

load_workers(Max) ->
	Workers = get_workers(),
	{Workers, load_workers(Max, Workers)}.

load_workers(Max, Workers) ->
	Rs = lists:reverse(ranges(Max, length(Workers))),
	lists:zip(Rs, lists:sublist(Workers, length(Rs))).

ranges(Max, Count) when Count > Max ->
	ranges(Max, Max);
ranges(Max, Count) ->
	Step = Max div Count,
	R = [{N * Step, N * Step + Step} || N <- lists:seq(Count - 1, 0, -1)],
	case Max rem Count of
		0 -> R;
		_ ->
			[{From, _To} | Tail] = R,
			[{From, Max} | Tail]
	end.


run_worker(Task) ->
	Self = self(),
	{spawn_link(fun() -> worker(Task, Self) end), Task}.

worker({{FromN, ToN}, Srv}, ReturnTo) ->
	ReturnTo ! {self(), test_results, basin_primes_srv:test_range({FromN, ToN}, Srv)}.

get_workers() ->
	catch net_adm:world(),
	{Srvs, _BadNodes} = rpc:multicall(basin_primes_sup, srvs, []),
	lists:flatten(Srvs).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

view_test() ->
	?debugFmt("~p~n", [ranges(101, 20)]).

view_2_test() ->
	?debugFmt("~p~n", [load_workers(101, lists:seq(1, 20))]).

-endif.
