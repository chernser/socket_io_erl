%%  Copyright (c) Sergey Chernov <chernser@outlook.com>
%%  All rights reserved.
-module(socket_io_erl_connection_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_connection/1]).

%% supervisor
-export([init/1]).

%%%============================================================================
%% API
%%%============================================================================

%% @hidden
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc starts new socket.io connection as supervisor child
-spec start_connection(SessionId::binary()) -> ok.
start_connection(SessionId) ->
  supervisor:start_child(?MODULE,
                         {{socket_io_erl_connection, SessionId},
                          {socket_io_erl_connection, start_link, [SessionId]},
                          permanent, 5000, worker, []}).

%%%============================================================================
%% supervisor callbacks
%%%============================================================================
init([]) ->
  {ok, {{one_for_one, 5, 10}, []}}.
