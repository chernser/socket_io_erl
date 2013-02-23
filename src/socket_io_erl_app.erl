%%  Copyright (c) Sergey Chernov <chernser@outlook.com>
%%  All rights reserved.

-module(socket_io_erl_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1]).

%%%============================================================================
%%% Application callbacks
%%%============================================================================

start(_StartType, _StartArgs) ->
    socket_io_erl_sup:start_link().

stop(_State) ->
    ok.
