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
%%% @doc Protocol Buffers Handler.
%%% @see
%%% <a href="https://github.com/extend/cowboy">Cowboy Sources</a>
%%% <a href="https://developers.google.com/protocol-buffers">
%%%     Google Protocol Buffers </a>
%%% @end
%%% Created : 22. Jul 2014 10:00 AM
%%%-------------------------------------------------------------------
-module(west_cowboy_ws_pb_handler).

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

-record(state, {server=?WEST_SERVER{}, nb_texts=0, nb_bins=0}).

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
    Dist      = application:get_env(west, dist, gproc),
    Scope     = ?GPROC_SCOPE(Dist),
    DistProps = application:get_env(west, dist_props, [{opts, [{n, 1}, {q, 1}]}]),
    case cowboy_req:binding(key, Req) of
        {Key, _} ->
            Name = west_utils:build_name([Key, self(), os:timestamp()]),
            register(Name, self()),
            CbSpec = {?MODULE, ev_callback, [{Name, node()}, undefined]},
            {ok, Req, #state{server=?WEST_SERVER{name=Name,
                                                 key=Key,
                                                 dist=Dist,
                                                 dist_props=DistProps,
                                                 scope=Scope,
                                                 cb=CbSpec,
                                                 format=pb}}};
        _ ->
            {shutdown, Req}
    end.

websocket_handle({binary, Msg},
                 Req,
                 #state{nb_bins=M, server=?WEST_SERVER{key=K}}=State) ->
    ?LOG_INFO("Received binary msg (M=~p): ~p bytes~n", [M, byte_size(Msg)]),
    try
        DecMsg = message_pb:decode_message(Msg),
        Cmd = west_utils:iolist_to_atom(DecMsg#message.event),
        ?LOG_INFO("[~p] ~p ~p~n",
                  [K, DecMsg#message.event, DecMsg#message.channel]),
        case west_protocol_handler:handle_event(Cmd,
                                                DecMsg,
                                                State#state.server) of
            {ok, Res} ->
                BinRes = iolist_to_binary(message_pb:encode_message(Res)),
                {reply, {binary, BinRes}, Req, State#state{nb_bins=M+1}};
            {error, Err} ->
                BinErr = iolist_to_binary(message_pb:encode_message(Err)),
                {reply, {binary, BinErr}, Req, State#state{nb_bins=M+1}};
            _ ->
                Err1 = ?RES_ACTION_NOT_ALLOWED(DecMsg#message.id,
                                               DecMsg#message.channel,
                                               pb),
                BinErr1 = iolist_to_binary(message_pb:encode_message(Err1)),
                {reply, {binary, BinErr1}, Req, State#state{nb_bins=M+1}}
        end
    catch
        _:_ ->
            Err2 = ?RES_BAD_REQUEST(pb),
            BinErr2 = iolist_to_binary(message_pb:encode_message(Err2)),
            {reply, {binary, BinErr2}, Req, State#state{nb_bins=M+1}}
    end;
websocket_handle({text, Msg}, Req, #state{nb_texts=N}=State) ->
    ?LOG_INFO("Received text msg (N=~p): ~p bytes~n", [N, byte_size(Msg)]),
    {reply, {text, <<"bad_encoding">>}, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({event, Msg}, Req, State) ->
    {reply, {binary, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(Reason, _Req, State) ->
    ?LOG_INFO("terminate ~p: ~p (state:~p)~n", [self(), Reason, State]),
    ok.

%%%===================================================================
%%% Callback
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Event callback. This function is executed when messages arrives.
%%
%% @end
%%--------------------------------------------------------------------
ev_callback({ETag, Event, Msg}, [WSRef, Id]) ->
    Reply = ?RES_CH_NEW_MSG(Id, ETag, atom_to_binary(Event, utf8), Msg, pb),
    BinReply = iolist_to_binary(message_pb:encode_message(Reply)),
    WSRef ! {event, BinReply}.
