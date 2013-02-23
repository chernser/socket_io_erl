%%  Copyright (c) Sergey Chernov <chernser@outlook.com>
%%  All rights reserved.

-module(socket_io_erl_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%============================================================================
%%% API functions
%%%============================================================================

-spec start_link() -> {ok, Pid::pid()} | 'ignore' | {error, Reason::any()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%============================================================================
%%% Supervisor callbacks
%%%============================================================================

init([]) ->
  ConnectionSupervisor = {socket_io_erl_connection_sup,
                          {socket_io_erl_connection_sup, start_link, []},
                          permanent, 5000, supervisor, []},

  {ok, {{one_for_one, 5, 10}, [ConnectionSupervisor]}}.

