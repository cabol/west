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
%%% @author Carlos Andres Bolaños R.A. <cabolanos@niagarasystems.co>
%%% @copyright (C) 2013, <Carlos Andres Bolaños>, All Rights Reserved.
%%% @doc Protocol Buffers Handler
%%% @see
%%% <a href="https://developers.google.com/protocol-buffers">
%%%     Google Protocol Buffers
%%% </a>
%%% @end
%%% Created : 13. Apr 2014 12:26 PM
%%%-------------------------------------------------------------------
-module(west_ws_pb_protocol_handler).

%% Export for websocket callbacks
-export([init/1,
         terminate/2,
         handle_open/2,
         handle_message/2,
         handle_info/2]).

%% Callback
-export([ev_callback/2]).

-include_lib("yaws/include/yaws_api.hrl").
-include("west.hrl").
-include("west_protocol.hrl").

-record(state, {server=?WEST_SERVER{}, nb_texts=0, nb_bins=0}).

%%%===================================================================
%%% WS callback
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initialize the internal state of the callback module.
%%
%% @see <a href="http://hyber.org/websockets.yaws">Yaws</a>
%%
%% @end
%%--------------------------------------------------------------------
init([Arg, InitialState]) ->
    ?LOG_INFO("Initalize ~p: ~p~n", [self(), InitialState]),
    Dist = case application:get_env(west, dist) of
               {ok, Env0} -> Env0;
               _          -> west_dist
           end,
    Scope = case Dist of
                gproc_dist -> g;
                _          -> l
            end,
    WDist = case application:get_env(west, west_dist) of
                {ok, Env1} -> Env1;
                _          -> [{n, 1}, {q, 1}]
            end,
    case string:tokens(Arg#arg.pathinfo, "/") of
        [Key] ->
            Name = west_utils:build_name([Key, self(), erlang:now()]),
            register(Name, self()),
            CbSpec = {?MODULE, ev_callback, [{Name, node()}, undefined]},
            {ok, #state{server=?WEST_SERVER{name=Name,
                                            key=Key,
                                            dist=Dist,
                                            west_dist=WDist,
                                            scope=Scope,
                                            cb = CbSpec,
                                            format=pb}}};
        _ ->
            Err = ?MSG{event = "bad_request",
                       data = <<"Error, missing key in path.">>},
            {error, iolist_to_binary(west_msg_pb:encode_message(Err))}
    end.

%%--------------------------------------------------------------------
%% @doc
%% This function is called when the connection is upgraded from HTTP
%% to WebSocket.
%%
%% @see <a href="http://hyber.org/websockets.yaws">Yaws</a>
%%
%% @end
%%--------------------------------------------------------------------
handle_open(WSState, State) ->
    Response = ?RES_CONN_ESTABLISHED(pb),
    ResBin = west_msg_pb:encode_message(Response),
    yaws_websockets:send(WSState, {binary, iolist_to_binary(ResBin)}),
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc
%% This function is called when a binary message is received.
%% {binary, Data} is the unfragmented binary message.
%% SUPPORTED by this handler.
%%
%% @see <a href="http://hyber.org/websockets.yaws">Yaws</a>
%%
%% @end
%%--------------------------------------------------------------------
handle_message({binary, Msg},
               #state{nb_bins=M, server=?WEST_SERVER{key=K}}=State) ->
    ?LOG_INFO("Received binary msg (M=~p): ~p bytes~n", [M, byte_size(Msg)]),
    try
        DecMsg = west_msg_pb:decode_message(Msg),
        Cmd = west_utils:iolist_to_atom(DecMsg#message.event),
        ?LOG_INFO("[~p] ~p ~p~n",
                  [K, DecMsg#message.event, DecMsg#message.channel]),
        case west_protocol_handler:handle_event(Cmd,
                                                DecMsg,
                                                State#state.server) of
            {ok, Res} ->
                BinRes = iolist_to_binary(west_msg_pb:encode_message(Res)),
                {reply, {binary, BinRes}, State#state{nb_bins=M+1}};
            {error, Err} ->
                BinErr = iolist_to_binary(west_msg_pb:encode_message(Err)),
                {reply, {binary, BinErr}, State#state{nb_bins=M+1}};
            _ ->
                Err1 = ?RES_ACTION_NOT_ALLOWED(DecMsg#message.id,
                                               DecMsg#message.channel,
                                               pb),
                BinErr1 = iolist_to_binary(west_msg_pb:encode_message(Err1)),
                {reply, {binary, BinErr1}, State#state{nb_bins=M+1}}
        end
    catch
        _:_ ->
            Err2 = ?RES_BAD_REQUEST(pb),
            BinErr2 = iolist_to_binary(west_msg_pb:encode_message(Err2)),
            {reply, {binary, BinErr2}, State#state{nb_bins=M+1}}
    end;

%%--------------------------------------------------------------------
%% @doc
%% This function is called when a text message is received.
%% {text, Data} is the unfragmented binary message.
%% NOT SUPPORTED by this handler.
%%
%% @see <a href="http://hyber.org/websockets.yaws">Yaws</a>
%%
%% @end
%%--------------------------------------------------------------------
handle_message({text, Msg}, #state{nb_texts=N}=State) ->
    ?LOG_INFO("Received text msg (N=~p): ~p bytes~n", [N, byte_size(Msg)]),
    {reply, {text, <<"Bad encoding.">>}, State#state{nb_bins=N+1}};

%%--------------------------------------------------------------------
%% @doc
%% When the client closes the connection, the callback module is
%% notified with the message {close, Status, Reason}
%%
%% @see <a href="http://hyber.org/websockets.yaws">Yaws</a>
%%
%% @end
%%--------------------------------------------------------------------
handle_message({close, Status, Reason}, _) ->
    ?LOG_INFO("Close connection: ~p - ~p~n", [Status, Reason]),
    {close, Status}.

%%--------------------------------------------------------------------
%% @doc
%% If defined, this function is called when a timeout occurs or when
%% the handling process receives any unknown message.
%%
%% Info is either the atom timeout, if a timeout has occurred, or
%% the received message.
%%
%% @see <a href="http://hyber.org/websockets.yaws">Yaws</a>
%%
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    ?LOG_INFO("process timed out~n", []),
    Bin = iolist_to_binary(west_msg_pb:encode_message(?MSG{event="timeout"})),
    {reply, {binary, Bin}, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% This function is called when the handling process is about to
%% terminate. it should be the opposite of Module:init/1 and do any
%% necessary cleaning up.
%%
%% @see <a href="http://hyber.org/websockets.yaws">Yaws</a>
%%
%% @end
%%--------------------------------------------------------------------
terminate(Reason, State) ->
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
    BinReply = iolist_to_binary(west_msg_pb:encode_message(Reply)),
    yaws_api:websocket_send(WSRef, {binary, BinReply}).
