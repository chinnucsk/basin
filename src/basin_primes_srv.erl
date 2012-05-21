-module(basin_primes_srv).

-behaviour(gen_server).

-export([start_link/1, test_range/4]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

test_range(FromN, ToN, Name, ReturnTo) when is_integer(FromN), is_integer(ToN) ->
	gen_server:cast(Name, {test_range, FromN, ToN, Name, ReturnTo}).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({test_range, FromN, ToN, Self, ReturnTo}, State) ->
	Reply = basin_primes:test(FromN, ToN),
	ReturnTo ! {Self, test_result, Reply},
	{noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


