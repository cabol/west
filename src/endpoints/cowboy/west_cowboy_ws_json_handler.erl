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
            Name = west_utils:build_name([Key, self(), erlang:now()]),
            register(Name, self()),
            CbSpec = {?MODULE, ev_callback, [{Name, node()}, undefined]},
            {ok, Req, #state{server=?WEST_SERVER{name=Name,
                                                 key=Key,
                                                 dist=Dist,
                                                 dist_props=DistProps,
                                                 scope=Scope,
                                                 cb=CbSpec,
                                                 format=json}}};
        _ ->
            {shutdown, Req}
    end.

websocket_handle({text, <<"bye">>}, Req, #state{nb_texts=N, nb_bins=M}=State) ->
    ?LOG_INFO("bye - Msg processed: ~p text, ~p binary~n", [N, M]),
    {shutdown, Req, State};
websocket_handle({text, Msg},
                 Req,
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
                    {reply, {text, Response}, Req, State#state{nb_texts=N+1}};
                {error, Err0} ->
                    {reply, {text, Err0}, Req, State#state{nb_texts=N+1}};
                _ ->
                    ?MSG{id=Id, channel=Ch} = ParsedMsg,
                    Err1 = ?RES_ACTION_NOT_ALLOWED(Id, Ch, json),
                    {reply, {text, Err1}, Req, State#state{nb_texts=N+1}}
            end
    end;
websocket_handle({binary, Msg}, Req, #state{nb_bins=M}=State) ->
    ?LOG_INFO("Received binary msg (M=~p): ~p bytes~n", [M, byte_size(Msg)]),
    {reply, {binary, <<"bad_encoding">>}, Req, State#state{nb_bins=M+1}};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({event, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
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
    Reply = ?RES_CH_NEW_MSG(Id, ETag, Event, Msg, json),
    WSRef ! {event, Reply}.
