%%  Copyright (c) Sergey Chernov <chernser@outlook.com>
%%  All rights reserved.
-module(socket_io_erl_handler).

%% Behaviour API
-export([behaviour_info/1]).


%% @hidden
behaviour_info(callbacks) ->
  [{check, 1}, {terminate, 1}, {send,2}];
behaviour_info(_) ->
  undefined.