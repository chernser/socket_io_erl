%%  Copyright (c) Sergey Chernov <chernser@outlook.com>
%%  All rights reserved.
-module(socket_io_erl_connection).

-behaviour(gen_fsm).

-include("../include/socket_io_erl.hrl").

%% API
-export([start_link/1, new/1, set_handler/1, unset_handler/1, send_msg/2,
  confirm_handler_check/1, migrate/2]).

%% gen_fsm callbacks
-export([init/1, connected/2, disconnected/2, handler_check/2, handle_event/3,
  handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% Time to wait for connection from client
%% After this time connection dies
-define(CONNECT_TIMEOUT, 30000).

%% Time  interval to check if handler still alive
%% After sending check message, handler should respond with check_ok message
-define(HANDLER_CHECK_INTERVAL, 2000).

%% Timeout for waiting handler response to check message
-define(HANDLER_CHECK_TIMEOUT, 500).


%% Types
-record(state, {
  handler :: pid(),
  lastMsgId :: integer(),
  messages :: list(),
  selfDestructionTimer :: term(),
  handlerCheckTimer :: term()
}).

%% ============================================================================
%% API
%% ============================================================================


%% @hidden
-spec start_link(SessionId :: binary()) -> any().
start_link(SessionId) ->
  gen_fsm:start_link({global, {?MODULE, SessionId}}, ?MODULE,
    [{sessionId, SessionId}], []).

%% @doc start new connection
-spec new(SessionId :: binary()) -> ok.
new(SessionId) ->
  socket_io_erl_connection_sup:start_connection(SessionId).

%% @doc attaches handler to connection. Previous handler will be killed
-spec set_handler(#socket_io_handler{}) -> ok.
set_handler(Handler = #socket_io_handler{sessionId = SessionId}) ->
  gen_fsm:send_event({global, {?MODULE, SessionId}}, {attach_handler, Handler}).

%% @doc detaches handler from connection. Connection will be switched to
%% disconnected state
-spec unset_handler(SessionId :: binary()) -> ok.
unset_handler(SessionId) ->
  gen_fsm:send_event({global, {?MODULE, SessionId}}, detach_handler).

%% @doc sends message to client. Message is #socket_io_msg{} record
-spec send_msg(SessionId :: binary(), Msg :: #socket_io_msg{}) -> ok.
send_msg(SessionId, Msg) ->
  gen_fsm:send_event({global, {?MODULE, SessionId}}, {send, Msg}).

%% @doc confirms success of handler check. Should be sent by handle in response
%% to check request
-spec confirm_handler_check(SessionId :: binary()) -> ok.
confirm_handler_check(SessionId) ->
  gen_fsm:send_event({global, {?MODULE, SessionId}}, check_ok).

%% @doc starts migration of connection to different node (Destination)
-spec migrate(SessionId :: binary(), Destination :: term()) -> ok.
migrate(SessionId, Destination) ->
  gen_fsm:send_event({global, {?MODULE, SessionId}}, {migrate, Destination}).

%% ============================================================================
%% gen_fsm
%% ============================================================================
init(_Args) ->
  io:fwrite("Connection Initialised with args: ~p~n", [_Args]),
  {ok, SelfDestructionTimer} = timer:send_after(?CONNECT_TIMEOUT, self(),
    close_connection),

  State0 = #state{
        lastMsgId = 0,
        messages = [],
        selfDestructionTimer = SelfDestructionTimer
  },

  {ok, disconnected, State0, ?CONNECT_TIMEOUT}.


disconnected(timeout, State) ->
  %% Send notification about dead connection and undelivered messages
  {stop, connect_timeouted, State};
disconnected({send, Msg}, State0) ->
  %% Accumulate messages to send in future
  Messages = State0#state.messages ++ [Msg],
  State1 = State0#state{messages = Messages},
  {next_state, disconnected, State1, ?CONNECT_TIMEOUT};
disconnected({attach_handler, Handler}, State0) ->
  %% Attach handler to current connection
  timer:cancel(State0#state.selfDestructionTimer),
  HandlerCheckTimer = gen_fsm:start_timer(?HANDLER_CHECK_INTERVAL, check_handler),
  State1 = State0#state{handler = Handler, selfDestructionTimer = undefined,
      handlerCheckTimer = HandlerCheckTimer},
  {next_state, connected, State1};
disconnected({migrate, _Destination}, State0) ->
  %% Transfer undelivered messages over wire to another instance - Destination
  {stop, connection_migrated, State0};
disconnected(_Event, State) ->
  {next_state, disconnected, State, ?CONNECT_TIMEOUT}.


connected(check_handler, State0) ->
  %% Request handler check
  Handler = State0#state.handler,
  HandlerModule = Handler#socket_io_handler.module,
  HandlerModule:check(Handler),
  {next_state, handler_check, State0, ?HANDLER_CHECK_TIMEOUT};
connected({send, Msg}, State0) ->
  %% Send single message to handler
  Handler = State0#state.handler,
  HandlerModule = Handler#socket_io_handler.module,
  EncodedMsg = socket_io_erl_messages:encode(Msg),
  HandlerModule:send(Handler, EncodedMsg),
  {next_state, connected, State0};
connected(detach_handler, State0) ->
  %% Detach handler and go to disconnected state
  State1 = detach_handler(State0),
  {next_state, disconnected, State1, ?CONNECT_TIMEOUT};
connected(_Event, State) ->
  {next_state, connected, State}.

handler_check(timeout, State0) ->
  %% Handler check failed - set connection disconnected
  State1 = detach_handler(State0),
  {next_state, disconnected, State1};
handler_check(check_ok, State0) ->
  %% Send accumulated messages and go to connected state
  {next_state, connected, State0};
handler_check({send, Msg}, State0) ->
  %% Accumulate messages to send in future
  Messages = State0#state.messages ++ [Msg],
  State1 = State0#state{messages = Messages},
  {next_state, handler_check, State1, ?HANDLER_CHECK_TIMEOUT};
handler_check(_Event, State) ->
  {next_state, handler_check, State}.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  {reply, ok, StateName, State}.

handle_info(close_connection, _, State) ->
  %% Close connection and clean up everything
  {stop, connection_closed, State};
handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.



%% ============================================================================
%% Internal Functions
%% ============================================================================


%% @hidden
-spec detach_handler(State0 :: #state{}) -> #state{}.
detach_handler(State0 = #state{}) ->
  gen_fsm:cancel_timer(State0#state.handlerCheckTimer),
  SelfDestructionTimer = timer:send_after(?CONNECT_TIMEOUT, self(),
    close_connection),
  State1 = State0#state{selfDestructionTimer = SelfDestructionTimer,
      handler = undefined,
      handlerCheckTimer = undefined},
  State1.