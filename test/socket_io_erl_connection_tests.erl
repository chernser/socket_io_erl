%% Copyright
-module(socket_io_erl_connection_tests).
-author("chernser").

-include_lib("eunit/include/eunit.hrl").


-include("socket_io_erl.hrl").
-include_lib("eunit/include/eunit.hrl").


-define(_ut(Test),
  fun(Fixture) ->
    {atom_to_list(Test), fun() -> Test(Fixture) end}
  end).

%% Common cleanup
cleanup(_) ->
  ok.


%% Testing connect to disconnect state
setup_disconnected() ->
  ok.

disconnected_to_connected_(_) ->
  {foreach, fun setup/0, fun cleanup/1,
    []
  }.



