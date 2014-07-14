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
%%% @doc JSON Wire Protocol. This module is the `yaws' extended
%%%      callback module. Here the WS messages are received and
%%%      handled.
%%% @see <a href="https://github.com/klacke/yaws">Yaws Sources</a>
%%% @end
%%% Created : 08. Nov 2013 3:05 AM
%%%-------------------------------------------------------------------
-module(west_ws_json_protocol_handler).

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
    DistProps = case application:get_env(west, dist_props) of
                    {ok, Env1} -> Env1;
                    _          -> [{opts, [{n, 1}, {q, 1}]}]
                end,
    case string:tokens(Arg#arg.pathinfo, "/") of
        [Key] ->
            Name = west_utils:build_name([Key, self(), erlang:now()]),
            register(Name, self()),
            CbSpec = {?MODULE, ev_callback, [{Name, node()}, undefined]},
            {ok, #state{server=?WEST_SERVER{name=Name,
                                            key=Key,
                                            dist=Dist,
                                            dist_props=DistProps,
                                            scope=Scope,
                                            cb=CbSpec,
                                            format=json}}};
        _ ->
            Err = "{\"event\":\"bad_request\", "
                    "\"data\":\"Error, missing key in path.\"}",
            {error, iolist_to_binary(Err)}
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
    Response = ?RES_CONN_ESTABLISHED(json),
    yaws_websockets:send(WSState, {text, Response}),
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc
%% This function is called when a message <<"bye">> ({text, Data}) is
%% received.
%%
%% @see <a href="http://hyber.org/websockets.yaws">Yaws</a>
%%
%% @end
%%--------------------------------------------------------------------
handle_message({text, <<"bye">>}, #state{nb_texts=N, nb_bins=M}=State) ->
    ?LOG_INFO("bye - Msg processed: ~p text, ~p binary~n", [N, M]),
    NbTexts = list_to_binary(integer_to_list(N)),
    NbBins = list_to_binary(integer_to_list(M)),
    Messages = [{text, <<"Goodbye !">>},
                {text, <<NbTexts/binary, " text messages received">>},
                {text, <<NbBins/binary, " binary messages received">>}],
    {close, {1000, <<"bye">>}, Messages, State};

%%--------------------------------------------------------------------
%% @doc
%% This function is called when a text message is received.
%% {text, Data} is the unfragmented binary message.
%% SUPPORTED by this handler.
%%
%% @see <a href="http://hyber.org/websockets.yaws">Yaws</a>
%%
%% @end
%%--------------------------------------------------------------------
handle_message({text, Msg},
               #state{nb_texts=N, server=?WEST_SERVER{key=K}}=State) ->
    ?LOG_INFO("Received text msg (N=~p): ~p bytes~n", [N, byte_size(Msg)]),
    case west_msg_utils:parse_msg(Msg) of
        {error, Reason} ->
            {reply, {text, Reason}, State#state{nb_texts=N+1}};
        ParsedMsg ->
            ?LOG_INFO("[~p] ~p ~p~n",
                      [K, ParsedMsg#message.event, ParsedMsg#message.channel]),
            Cmd = binary_to_atom(ParsedMsg#message.event, utf8),
            case west_protocol_handler:handle_event(Cmd,
                                                    ParsedMsg,
                                                    State#state.server) of
                {ok, Response} ->
                    {reply, {text, Response}, State#state{nb_texts=N+1}};
                {error, Err0} ->
                    {reply, {text, Err0}, State#state{nb_texts=N+1}};
                _ ->
                    ?MSG{id=Id, channel=Ch} = ParsedMsg,
                    Err1 = ?RES_ACTION_NOT_ALLOWED(Id, Ch, json),
                    {reply, {text, Err1}, State#state{nb_texts=N+1}}
            end
    end;

%%--------------------------------------------------------------------
%% @doc
%% This function is called when a binary message is received.
%% {binary, Data} is the unfragmented binary message.
%% NOT SUPPORTED by this handler.
%%
%% @see <a href="http://hyber.org/websockets.yaws">Yaws</a>
%%
%% @end
%%--------------------------------------------------------------------
handle_message({binary, Msg}, #state{nb_bins=M}=State) ->
    ?LOG_INFO("Received binary msg (M=~p): ~p bytes~n", [M, byte_size(Msg)]),
    {reply, {binary, <<"Bad encoding.">>}, State#state{nb_bins=M+1}};

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
    {reply, {text, <<"{\"event\":\"timeout\"}">>}, State};
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
    Reply = ?RES_CH_NEW_MSG(Id, ETag, Event, Msg, json),
    yaws_api:websocket_send(WSRef, {text, Reply}).
