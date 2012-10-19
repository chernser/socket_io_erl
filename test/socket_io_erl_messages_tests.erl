-module(socket_io_erl_messages_tests).


-include("socket_io_erl.hrl").
-include_lib("eunit/include/eunit.hrl").



%% Tests decoding
decode_test() ->
  #socket_io_msg{type = ?SOCKET_IO_MSG_CONNECT} = socket_io_erl_messages:decode(<<"1::">>),
  #socket_io_msg{type = ?SOCKET_IO_MSG_CONNECT, endpoint = <<"/test?key=1">>} = socket_io_erl_messages:decode(<<"1::/test?key=1">>),
  #socket_io_msg{type = ?SOCKET_IO_MSG_HEARTBEAT} = socket_io_erl_messages:decode(<<"2::">>),
  #socket_io_msg{type = ?SOCKET_IO_MSG_ACK} = socket_io_erl_messages:decode(<<"6:::1">>),
  #socket_io_msg{type = ?SOCKET_IO_MSG_MESSAGE, endpoint = <<"/chat">>, data = <<"Hello">>} = socket_io_erl_messages:decode(<<"3::/chat:Hello">>),
  ok.

%% Tests encoding
encode_test() ->
  <<"1::">> = socket_io_erl_messages:encode(#socket_io_msg{type = ?SOCKET_IO_MSG_CONNECT}),
  <<"1::/chat">> =socket_io_erl_messages:encode(#socket_io_msg{type = ?SOCKET_IO_MSG_CONNECT, endpoint = <<"/chat">>}),
  <<"2::">> = socket_io_erl_messages:encode(#socket_io_msg{type = ?SOCKET_IO_MSG_HEARTBEAT}),
  ok.