
%% Message types accordint to https://github.com/LearnBoost/socket.io-spec
-define(SOCKET_IO_MSG_CONNECT, <<"1">>).
-define(SOCKET_IO_MSG_HEARTBEAT, <<"2">>).
-define(SOCKET_IO_MSG_MESSAGE, <<"3">>).
-define(SOCKET_IO_MSG_JSON_MESSAGE, <<"4">>).
-define(SOCKET_IO_MSG_EVENT, <<"5">>).
-define(SOCKET_IO_MSG_ACK, <<"6">>).
-define(SOCKET_IO_MSG_ERROR, <<"7">>).
-define(SOCKET_IO_MSG_NOOP, <<"8">>).

%% @doc Holding parsed socket.io message
-record(socket_io_msg, {
  type,
  id,
  endpoint,
  data
}).


%% @doc describes handler
-record(socket_io_handler, {
  sessionId,
  type,
  module,
  ref
}).