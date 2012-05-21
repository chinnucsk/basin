-module(basin_primes_sup).

-behaviour(supervisor).

-export([start_link/0, srvs/0]).

-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

-define(BPSCHILD(Id), ?CHILD(Id, basin_primes_srv, worker, [Id])).

srvs() ->
	[{X, node()} || {X, _Y, _Z, _Q} <- supervisor:which_children(?MODULE)].

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Children = [?BPSCHILD(list_to_atom(lists:flatten(io_lib:format("primes_srv~p", [N])))) || N <- lists:seq(0, srvs_count() - 1)],
    {ok, {{one_for_one, 5, 10}, Children}}.

srvs_count() ->
	case application:get_env(primes_srvs) of
		{ok, Val} -> Val;
		_ -> 20
	end.
