%%  Copyright (c) Sergey Chernov <chernser@outlook.com>
%%  All rights reserved.
-module(socket_io_erl_test_http_handler).
-behaviour(socket_io_erl_handler).

-include_lib("socket_io_erl/include/socket_io_erl.hrl").

%% API
-export([check/1,
         terminate/1,
         send/2]).


check(#socket_io_handler{sessionId = _SessionId}) ->
  ok.

terminate(_Ctx) ->
  ok.

send(_SessionId, _Message) ->
  ok.