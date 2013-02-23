%%  Copyright (c) Sergey Chernov <chernser@outlook.com>
%%  All rights reserved.
-module(socket_io_erl_connection_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("socket_io_erl/include/socket_io_erl.hrl").

-define(SID, <<"test-dummy-session">>).
-define(HANDLER_MODULE, socket_io_erl_test_http_handler).
-define(_test2(Test),
  fun(Fixture) ->
    {atom_to_list(Test), fun() -> Test(Fixture) end}
  end).

setup() ->
  meck:new(?HANDLER_MODULE, [stub_all, passthrough]),
  {ok, ConnectionSupPid} = socket_io_erl_connection_sup:start_link(),
  erlang:unlink(ConnectionSupPid),
  {ok, ConnectionPid}= socket_io_erl_connection:new(?SID),
  {[ConnectionSupPid], ConnectionPid}.

cleanup({PidsToKill, _}) ->

  %% Kill processes
  erlang:process_flag(trap_exit, true),
  lists:foreach(
    fun(Pid) ->
        erlang:exit(Pid, shutdown),
        receive
          {'EXIT', Pid, _} ->
            ok
          after
            600 ->
              erlang:exit(Pid, kill)
          end
    end,
    PidsToKill),
  erlang:process_flag(trap_exit, false),
  meck:unload(),
  ok.

all_test_() ->
  {foreach, fun setup/0, fun cleanup/1,
    [?_test2(test_setting_handler),
     ?_test2(test_handler_check),
     ?_test2(test_sending_message_while_handler_check)]
  }.


%%%============================================================================
%%% Tests
%%%============================================================================

test_setting_handler(_) ->
  InitialState = socket_io_erl_connection:get_state(?SID),
  Handler = #socket_io_handler{module = ?HANDLER_MODULE,
                               sessionId = ?SID},
  socket_io_erl_connection:set_handler(Handler),
  State = socket_io_erl_connection:get_state(?SID),
  [?assertEqual(disconnected, InitialState),
   ?assertEqual(connected, State)].

test_handler_check(_) ->
  InitialState = socket_io_erl_connection:get_state(?SID),
  Handler = #socket_io_handler{module = ?HANDLER_MODULE,
                               sessionId = ?SID,
                               ref = some_pid},
  socket_io_erl_connection:set_handler(Handler),
  State = socket_io_erl_connection:get_state(?SID),
  timer:sleep(2100),
  true = meck:called(?HANDLER_MODULE, check, [Handler]),
  CheckState = socket_io_erl_connection:get_state(?SID),
  socket_io_erl_connection:confirm_handler_check(?SID),
  FinalState = socket_io_erl_connection:get_state(?SID),
  [?assertEqual(disconnected, InitialState),
   ?assertEqual(connected, State),
   ?assertEqual(handler_check, CheckState),
   ?assertEqual(connected, FinalState)].


test_sending_message_while_handler_check(_) ->
  Handler = #socket_io_handler{module = ?HANDLER_MODULE,
                               sessionId = ?SID},
  socket_io_erl_connection:set_handler(Handler),
  timer:sleep(2050),
  N = 4,
  Messages = [#socket_io_msg{type = ?SOCKET_IO_MSG_EVENT,
                             data = <<"{}">>,
                             id = INDEX} || INDEX <- lists:seq(1, N)],
  [socket_io_erl_connection:send_msg(?SID, Msg) || Msg <- Messages],
  socket_io_erl_connection:confirm_handler_check(?SID),
  timer:sleep(100),
  ?assert(meck:called(?HANDLER_MODULE, send, [?SID, Messages])).
