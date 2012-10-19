%% @doc
%%  Maintains connection state logic
%% @end
-module(socket_io_erl_connection).

-behaviour(gen_fsm).

%% API
-export([start_link/0]).

%% gen_fsm
-export([init/1, connected/2, disconnected/2, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% API
start_link() ->
  gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_fsm callbacks
-record(state, {
  handler :: pid(),
  lastMsgId :: integer(),
  messages :: list()
}).

init(_Args) ->
  State = #state{
      lastMsgId = 0,
      messages = []
  },
  {ok, disconnected, State}.


disconnected({send, Msg}, State0) ->
  Messages = State0#state.messages ++ [Msg],
  {next_state, disconnected, State0#state{messages = Messages}};
disconnected({attach_handler, Handler}, State0) ->
  {next_state, connected, State0#state{handler = Handler}, 0};
disconnected(_Event, State) ->
  {next_state, state_name, State}.


connected(timeout, State) ->
  {next_state, connected, State, 1000}.

connected(_Event, State) ->
  {next_state, state_name, State}.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.
