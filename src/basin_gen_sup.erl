-module(basin_gen_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     temporary, 5000, Type, [Mod]}).
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 5, 10}, [?CHILD(basin_gen_srv, basin_gen_srv, worker, [])]}}.

