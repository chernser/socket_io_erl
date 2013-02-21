-module(socket_io_erl_messages_tests).


-include("socket_io_erl.hrl").
-include_lib("eunit/include/eunit.hrl").


-define(_ut(Test),
  fun(Fixture) ->
    {atom_to_list(Test), fun() -> Test(Fixture) end}
  end).

setup() ->
  ok.

cleanup(_) ->
  ok.

all_test_() ->
  {foreach, fun setup/0, fun cleanup/1, [
    ?_ut(test_decoding),
    ?_ut(test_encoding)
  ]}.

%% Tests decoding
test_decoding(_) ->
  [
    ?assertEqual(#socket_io_msg{type = ?SOCKET_IO_MSG_CONNECT},
      socket_io_erl_messages:decode(<<"1::">>)),

    ?assertEqual(#socket_io_msg{type = ?SOCKET_IO_MSG_CONNECT,
        endpoint = <<"/test?key=1">>},
      socket_io_erl_messages:decode(<<"1::/test?key=1">>)),

    ?assertEqual(#socket_io_msg{type = ?SOCKET_IO_MSG_HEARTBEAT},
      socket_io_erl_messages:decode(<<"2::">>)),

    ?assertEqual(#socket_io_msg{type = ?SOCKET_IO_MSG_ACK, data = <<"1">>},
      socket_io_erl_messages:decode(<<"6:::1">>)),

    ?assertEqual(#socket_io_msg{type = ?SOCKET_IO_MSG_MESSAGE,
        endpoint = <<"/chat">>,
        data = <<"Hello">>},
      socket_io_erl_messages:decode(<<"3::/chat:Hello">>))
  ].

%% Tests encoding
test_encoding(_) ->
  [
    ?assertEqual(<<"1::">>,
      socket_io_erl_messages:encode(#socket_io_msg{type = ?SOCKET_IO_MSG_CONNECT})),
    ?assertEqual(<<"1::/chat">>,
      socket_io_erl_messages:encode(#socket_io_msg{type = ?SOCKET_IO_MSG_CONNECT,
        endpoint = <<"/chat">>})),
    ?assertEqual(<<"2::">>,
      socket_io_erl_messages:encode(#socket_io_msg{type = ?SOCKET_IO_MSG_HEARTBEAT}))
  ].
