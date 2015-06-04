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
%%% @doc JSON Wire Protocol.
%%% @see <a href="https://github.com/extend/cowboy">Cowboy Sources</a>
%%% @end
%%% Created : 23. Jul 2014 2:15 PM
%%%-------------------------------------------------------------------
-module(west_cowboy_ws_json_handler).

-behaviour(cowboy_websocket_handler).

%% Export for websocket callbacks
-export([init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

%% Callback
-export([ev_callback/2]).

-include("west.hrl").
-include("../../west_protocol.hrl").

-record(state, {server = ?WEST{}, nb_texts = 0, nb_bins = 0}).

%%%===================================================================
%%% WS callback
%%%===================================================================

init({_, http}, Req, _Opts) ->
  case application:get_env(west, http_ws_handshake_callback) of
    {ok, {Mod, Fun}} when Mod =/= none, Fun =/= none ->
      ?LOG_INFO("apply(~p, ~p)~n", [Mod, Fun]),
      case apply(Mod, Fun, [Req]) of
        ok ->
          {upgrade, protocol, cowboy_websocket};
        {Rc, Rp} when is_integer(Rc), is_binary(Rp) ->
          {ok, Req2} = cowboy_req:reply(Rc, [], Rp, Req),
          {ok, Req2, #state{}};
        _ ->
          {ok, Req3} = cowboy_req:reply(401, [], <<>>, Req),
          {ok, Req3, #state{}}
      end;
    _ ->
      {upgrade, protocol, cowboy_websocket}
  end.

websocket_init(_TransportName, Req, _Opts) ->
  ?LOG_INFO("Initalize ~p: ~p~n", [self(), Req]),
  Dist = application:get_env(west, dist, gproc),
  Scope = ?GPROC_SCOPE(Dist),
  DistProps = application:get_env(west, dist_props, [{opts, [{n, 1}, {q, 1}]}]),
  case cowboy_req:binding(key, Req) of
    {Key, _} ->
      Name = west_util:build_name([Key, self(), west_util:get_timestamp_ms()]),
      register(Name, self()),
      CbSpec = {?MODULE, ev_callback, [{Name, node()}, undefined]},
      {ok, Req, #state{server = ?WEST{name = Name,
                                             key = Key,
                                             dist = Dist,
                                             dist_props = DistProps,
                                             scope = Scope,
                                             cb = CbSpec,
                                             encoding = json}}};
    _ ->
      {shutdown, Req}
  end.

websocket_handle({text, <<"bye">>}, Req, #state{nb_texts = N, nb_bins = M} = S) ->
  ?LOG_INFO("bye - Msg processed: ~p text, ~p binary~n", [N, M]),
  {shutdown, Req, S};
websocket_handle({text, Msg},
                 Req,
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
          {reply, {text, Response}, Req, S#state{nb_texts = N + 1}};
        {error, Err0} ->
          {reply, {text, Err0}, Req, S#state{nb_texts = N + 1}};
        _ ->
          ?MSG{id = Id, channel = Ch} = ParsedMsg,
          Err1 = ?RES_ACTION_NOT_ALLOWED(Id, Ch, json),
          {reply, {text, Err1}, Req, S#state{nb_texts = N + 1}}
      end
  end;
websocket_handle({binary, Msg}, Req, #state{nb_bins = M} = S) ->
  ?LOG_INFO("Received binary msg (M=~p): ~p bytes~n", [M, byte_size(Msg)]),
  {reply, {binary, <<"bad_encoding">>}, Req, S#state{nb_bins = M + 1}};
websocket_handle(_Data, Req, S) ->
  {ok, Req, S}.

websocket_info({event, Msg}, Req, S) ->
  {reply, {text, Msg}, Req, S};
websocket_info(_Info, Req, S) ->
  {ok, Req, S}.

websocket_terminate(Reason, _Req, S) ->
  ?LOG_INFO("terminate ~p: ~p (state:~p)~n", [self(), Reason, S]),
  ok.

%%%===================================================================
%%% Callback
%%%===================================================================

%% @private
%% @doc Event callback. This function is executed when message arrives.
ev_callback({ETag, Event, Msg}, [WSRef, Id]) ->
  Reply = ?RES_CH_NEW_MSG(Id, ETag, Event, Msg, json),
  WSRef ! {event, Reply}.
