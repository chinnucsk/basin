-module(cluster).
-export([slaves/1]).

slaves([]) ->
	ok;

slaves([Host|Hosts]) ->
  Args = erl_system_args(),
  NodeName = "cluster",
  {ok, Node} = slave:start(list_to_atom(Host), NodeName, Args),
  io:format("Erlang node started = [~p]~n", [Node]),
  slaves(Hosts).

erl_system_args()->
  Shared = case init:get_argument(shared) of
    error -> " ";
    {ok,[[]]} -> " -shared "
  end,
  lists:append([" -rsh ssh -setcookie ",
                atom_to_list(erlang:get_cookie()),
                Shared, " -pa ~/basin/ebin/ -boot start_sasl -run basin start "]).

