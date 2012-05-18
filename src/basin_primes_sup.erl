-module(basin_primes_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, srvs/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

-define(BPSCHILD(Id), ?CHILD(Id, basin_primes_srv, worker, [Id])).

%%%===================================================================
%%% API functions
%%%===================================================================

srvs() ->
	[{X, node()} || {X, _Y, _Z, _Q} <- supervisor:which_children(?MODULE)].

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
	Children = [?BPSCHILD(list_to_atom(lists:flatten(io_lib:format("primes_srv~p", [N])))) || N <- lists:seq(0, srvs_count() - 1)],
    {ok, {{one_for_one, 5, 10}, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
srvs_count() ->
	case application:get_env(primes_srvs) of
		{ok, Val} -> Val;
		_ -> 20
	end.
