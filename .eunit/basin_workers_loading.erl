-module(basin_workers_loading).

-export([load_workers/2]).

-ifdef(TEST).
-compile(export_all).
-endif.

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


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

view_test() ->
	?debugFmt("~p~n", [ranges(101, 20)]).

view_2_test() ->
	?debugFmt("~p~n", [load_workers(101, lists:seq(1, 20))]).

-endif.
