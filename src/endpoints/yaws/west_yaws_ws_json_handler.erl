%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Carlos Andres Bolaños, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @author Carlos Andres Bolaños R.A. <candres@niagara.io>
%%% @copyright (C) 2013, <Carlos Andres Bolaños>, All Rights Reserved.
%%% @doc JSON Wire Protocol. This module is the `yaws' extended
%%%      callback module. Here the WS messages are received and
%%%      handled.
%%% @see <a href="https://github.com/klacke/yaws">Yaws Sources</a>
%%% @end
%%% Created : 08. Nov 2013 3:05 AM
%%%-------------------------------------------------------------------
-module(west_yaws_ws_json_handler).

%% Export for websocket callbacks
-export([init/1,
         terminate/2,
         handle_open/2,
         handle_message/2,
         handle_info/2]).

%% Callback
-export([ev_callback/2]).

-include("west.hrl").
-include("../../west_protocol.hrl").

-record(state, {server = ?WEST{}, nb_texts = 0, nb_bins = 0}).

%%%===================================================================
%%% WS callback
%%%===================================================================

%% @doc Initialize the internal state of the callback module.
%% @see <a href="http://hyber.org/websockets.yaws">Yaws</a>
init([Arg, InitialState]) ->
  ?LOG_INFO("Initalize ~p: ~p~n", [self(), InitialState]),
  Dist = application:get_env(west, dist, gproc),
  Scope = ?GPROC_SCOPE(Dist),
  DistProps = application:get_env(west, dist_props, [{opts, [{n, 1}, {q, 1}]}]),
  case string:tokens(yaws_api:arg_pathinfo(Arg), "/") of
    [_, Key] ->
      Name = west_util:build_name([Key, self(), west_util:get_timestamp_ms()]),
      register(Name, self()),
      CbSpec = {?MODULE, ev_callback, [{Name, node()}, undefined]},
      {ok, #state{server = ?WEST{name = Name,
                                        key = Key,
                                        dist = Dist,
                                        dist_props = DistProps,
                                        scope = Scope,
                                        cb = CbSpec,
                                        encoding = json}}};
    _ ->
      Err = "{\"event\":\"bad_request\", "
      "\"data\":\"Error, missing key in path.\"}",
      {error, iolist_to_binary(Err)}
  end.

%% @doc This function is called when the connection is upgraded from
%%      HTTP to WebSocket.
%% @see <a href="http://hyber.org/websockets.yaws">Yaws</a>
handle_open(WSState, S) ->
  Response = ?RES_CONN_ESTABLISHED(json),
  yaws_websockets:send(WSState, {text, Response}),
  {ok, S}.

%% @doc This function is called when a message <<"bye">> is received.
%% @see <a href="http://hyber.org/websockets.yaws">Yaws</a>
handle_message({text, <<"bye">>}, #state{nb_texts = N, nb_bins = M} = S) ->
  ?LOG_INFO("bye - Msg processed: ~p text, ~p binary~n", [N, M]),
  NbTexts = list_to_binary(integer_to_list(N)),
  NbBins = list_to_binary(integer_to_list(M)),
  Messages = [{text, <<"Goodbye !">>},
              {text, <<NbTexts/binary, " text messages received">>},
              {text, <<NbBins/binary, " binary messages received">>}],
  {close, {1000, <<"bye">>}, Messages, S};

%% @doc This function is called when a TEXT message is received.
%% @see <a href="http://hyber.org/websockets.yaws">Yaws</a>
handle_message({text, Msg},
#state{nb_texts = N, server = ?WEST{key = K}} = S) ->
  ?LOG_INFO("Received text msg (N=~p): ~p bytes~n", [N, byte_size(Msg)]),
  case west_msg:dec_msg(Msg, json) of
    {error, Reason} ->
      {reply, {text, Reason}, S#state{nb_texts = N + 1}};
    ParsedMsg ->
      ?LOG_INFO(
        "[~p] ~p ~p~n",
        [K, ParsedMsg#msg_t.event, ParsedMsg#msg_t.channel]),
      Cmd = binary_to_atom(ParsedMsg#msg_t.event, utf8),
      case west_protocol_handler:handle_event(Cmd, ParsedMsg, S#state.server) of
        {ok, Response} ->
          {reply, {text, Response}, S#state{nb_texts = N + 1}};
        {error, Err0} ->
          {reply, {text, Err0}, S#state{nb_texts = N + 1}};
        _ ->
          ?MSG{id = Id, channel = Ch} = ParsedMsg,
          Err1 = ?RES_ACTION_NOT_ALLOWED(Id, Ch, json),
          {reply, {text, Err1}, S#state{nb_texts = N + 1}}
      end
  end;

%% @doc This function is called when a binary message is received.
%%      NOT HANDLED by this handler.
%% @see <a href="http://hyber.org/websockets.yaws">Yaws</a>
handle_message({binary, Msg}, #state{nb_bins = M} = S) ->
  ?LOG_INFO("Received binary msg (M=~p): ~p bytes~n", [M, byte_size(Msg)]),
  {reply, {binary, <<"bad_encoding">>}, S#state{nb_bins = M + 1}};

%% @doc When the client closes the connection, the callback module is
%%      notified with the message {close, Status, Reason}
%% @see <a href="http://hyber.org/websockets.yaws">Yaws</a>
handle_message({close, Status, Reason}, _) ->
  ?LOG_INFO("Close connection: ~p - ~p~n", [Status, Reason]),
  {close, Status}.

%% @doc
%% If defined, this function is called when a timeout occurs or when
%% the handling process receives any unknown message.
%% <br/>
%% Info is either the atom timeout, if a timeout has occurred, or
%% the received message.
%% @see <a href="http://hyber.org/websockets.yaws">Yaws</a>
handle_info(timeout, S) ->
  ?LOG_INFO("process timed out~n", []),
  {reply, {text, <<"{\"event\":\"timeout\"}">>}, S};
handle_info(_Info, S) ->
  {noreply, S}.

%% @doc This function is called when the handling process is about to
%%      terminate. it should be the opposite of Module:init/1 and do
%%      any necessary cleaning up.
%% @see <a href="http://hyber.org/websockets.yaws">Yaws</a>
terminate(Reason, S) ->
  ?LOG_INFO("terminate ~p: ~p (state:~p)~n", [self(), Reason, S]),
  ok.

%%%===================================================================
%%% Callback
%%%===================================================================

%% @private
%% @doc Event callback. This function is executed when message arrives.
ev_callback({ETag, Event, Msg}, [WSRef, Id]) ->
  Reply = ?RES_CH_NEW_MSG(Id, ETag, Event, Msg, json),
  yaws_api:websocket_send(WSRef, {text, Reply}).
