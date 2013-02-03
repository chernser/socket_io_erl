-module(socket_io_erl_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  ConnectionSupervisor = {
    socket_io_erl_connection_sup,
    {socket_io_erl_connection_sup, start_link, []},
    permanent, 5000, supervisor, []
  },
  {ok, {{one_for_one, 5, 10}, [ConnectionSupervisor]}}.

