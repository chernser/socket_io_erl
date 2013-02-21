%%  Copyright (c) Sergey Chernov <chernser@outlook.com>
%%  All rights reserved.
-module(socket_io_erl_connection_sup).
-author("chernser").

-behaviour(supervisor).

%% API
-export([start_link/0, start_connection/1]).

%% supervisor
-export([init/1]).

%% ============================================================================
%% API
%% ============================================================================

%% @hidden
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc starts new socket.io connection as supervisor child
-spec start_connection(SessionId :: binary()) -> ok.
start_connection(SessionId) ->
  {ok, _} = supervisor:start_child({
    {socket_io_erl_connection, SessionId},
    {socket_io_erl_connection, start_link, [SessionId]},
    permanent, 5000, worker, dynamic
  }),
  ok.

%% ============================================================================
%% supervisor callbacks
%% ============================================================================
init([]) ->
  {ok, {{simple_one_for_one, 5, 10}, []}}.
