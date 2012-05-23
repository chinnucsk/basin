-module(basin_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 5, 10}, [?CHILD(basin_primes_sup, basin_primes_sup, supervisor, []),
								 ?CHILD(basin_gen_sup, basin_gen_sup, supervisor, [])]}}.

