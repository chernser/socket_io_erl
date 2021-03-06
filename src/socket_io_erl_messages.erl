%%  Copyright (c) Sergey Chernov <chernser@outlook.com>
%%  All rights reserved.

%% @doc
%%   Provides functions for encoding/decoding socket.io protocol messages
%% @end

-module(socket_io_erl_messages).

%% @headerfile "socket_io_erl.hrl"
-include_lib("socket_io_erl/include/socket_io_erl.hrl").

%% API
-export([decode/1,
         encode/1,
         getEventName/1,
         getEventArgs/1]).

%%%============================================================================
%%% API
%%%============================================================================

%% @doc Decodes message into record
-spec decode(binary() | list()) -> #socket_io_msg{}.
decode(Msg) when is_list(Msg) ->
  decode(list_to_binary(Msg));
decode(Msg) when is_binary(Msg) ->
  Parts = [ case Part of
                <<>> -> undefined;
                _ -> Part
            end || Part <- re:split(Msg, <<":">>, [{return, binary}])],
  parse(Parts).

%% @doc Encodes message record into binary()
%% @todo optimize
-spec encode(#socket_io_msg{}) -> binary().
encode(Msg) ->
  [socket_io_msg | Parts] = erlang:tuple_to_list(Msg),
  Encoded = << <<(if is_binary(P) -> P;
                     P =:= undefined -> <<"">>;
                     true -> erlang:term_to_binary(P)
                     end)/bitstring,
                      ":" >> || P <- Parts>>,
  Size = size(Encoded),
  case Msg#socket_io_msg.data of
     undefined ->
       binary:part(Encoded, 0, Size - 2);
     _ ->
       binary:part(Encoded, 0, Size - 1)
  end.

%% @doc Returns event name if message is event
-spec getEventName(#socket_io_msg{data::list()}) -> binary().
getEventName(#socket_io_msg{type = ?SOCKET_IO_MSG_EVENT, data = undefined}) ->
   undefined;
getEventName(#socket_io_msg{type = ?SOCKET_IO_MSG_EVENT, data = Data}) ->
   proplists:get_value(<<"name">>, Data);
getEventName(_) ->
    not_event.

%% @doc Returns event arguments if message is event
-spec getEventArgs(#socket_io_msg{data::list()}) -> binary().
getEventArgs(#socket_io_msg{type = ?SOCKET_IO_MSG_EVENT, data = undefined}) ->
  undefined;
getEventArgs(#socket_io_msg{type = ?SOCKET_IO_MSG_EVENT, data = Data}) ->
    proplists:get_value(<<"args">>, Data);
getEventArgs(_) ->
    not_event.

%% @doc Parses list of message parts into record
-spec parse([binary()]) -> #socket_io_msg{}.
parse([Type]) ->
  #socket_io_msg{type = Type};
parse([Type, Id, Endpoint]) ->
  #socket_io_msg{type = Type, id = Id, endpoint = Endpoint};
parse([Type, Id, Endpoint, Data]) ->
  #socket_io_msg{type = Type, id = Id, endpoint = Endpoint, data = Data}.
