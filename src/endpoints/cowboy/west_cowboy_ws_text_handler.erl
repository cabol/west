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
%%% @doc Text Wire Protocol.
%%% @see <a href="https://github.com/extend/cowboy">Cowboy Sources</a>
%%% @end
%%% Created : 17. Jul 2014 10:14 AM
%%%-------------------------------------------------------------------
-module(west_cowboy_ws_text_handler).

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
                                                 format=text}}};
        _ ->
            {shutdown, Req}
    end.

websocket_handle({text, <<"bye">>}, Req, #state{nb_texts=N, nb_bins=M}=State) ->
    ?LOG_INFO("bye - Msg processed: ~p text, ~p binary~n", [N, M]),
    {shutdown, Req, State};
websocket_handle({text, <<"ping">>}, Req, State) ->
    {reply, {text, <<"pong">>}, Req, State};
websocket_handle({text, Msg}, Req, #state{nb_texts=N}=State) ->
    ?LOG_INFO("Received text msg (N=~p): ~p bytes~n", [N, byte_size(Msg)]),
    case parse_msg(Msg) of
        none ->
            {reply, {text, Msg}, State#state{nb_texts=N+1}};
        Cmd ->
            case handle_event(string:to_lower(Cmd), State#state.server) of
                {ok, Reason} ->
                    {reply, {text, Reason}, Req, State#state{nb_texts=N+1}};
                {error, Err0} ->
                    {reply, {text, Err0}, Req, State#state{nb_texts=N+1}};
                _ ->
                    ErrMsg = <<"west:action_not_allowed">>,
                    {reply, {text, ErrMsg}, Req, State#state{nb_texts=N+1}}
            end
    end;
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
%%% Event handlers
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle the register event.
%%
%% @end
%%--------------------------------------------------------------------
handle_event(["reg", Ch], WS) ->
    MsgSpec = ?MSG{id=undefined, channel=Ch},
    Res = west_protocol_handler:handle_event(register, MsgSpec, WS),
    {_, ?MSG{event=Event}} = Res,
    BinRes = <<(<<"west ">>)/binary,
               (iolist_to_binary(Ch ++ ":"))/binary,
               (iolist_to_binary(Event))/binary>>,
    {ok, BinRes};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle the unregister event.
%%
%% @end
%%--------------------------------------------------------------------
handle_event(["unreg", Ch], WS) ->
    MsgSpec = ?MSG{id=undefined, channel=Ch},
    Res = west_protocol_handler:handle_event(unregister, MsgSpec, WS),
    {_, ?MSG{event=Event}} = Res,
    BinRes = <<(<<"west ">>)/binary,
               (iolist_to_binary(Ch ++ ":"))/binary,
               (iolist_to_binary(Event))/binary>>,
    {ok, BinRes};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle the send event.
%%
%% @end
%%--------------------------------------------------------------------
handle_event(["send", Ch, Msg], WS) ->
    MsgSpec = ?MSG{id=undefined, channel=Ch, data=Msg},
    Res = west_protocol_handler:handle_event(send, MsgSpec, WS),
    {_, ?MSG{event=Event}} = Res,
    BinRes = <<(<<"west ">>)/binary,
               (iolist_to_binary(Ch ++ ":"))/binary,
               (iolist_to_binary(Event))/binary>>,
    {ok, BinRes};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle the publish event.
%%
%% @end
%%--------------------------------------------------------------------
handle_event(["pub", Ch, Msg], WS) ->
    MsgSpec = ?MSG{id=undefined, channel=Ch, data=Msg},
    Res = west_protocol_handler:handle_event(publish, MsgSpec, WS),
    {_, ?MSG{event=Event}} = Res,
    BinRes = <<(<<"west ">>)/binary,
               (iolist_to_binary(Ch ++ ":"))/binary,
               (iolist_to_binary(Event))/binary>>,
    {ok, BinRes};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle the subscribe event.
%%
%% @end
%%--------------------------------------------------------------------
handle_event(["sub", Ch], WS) ->
    MsgSpec = ?MSG{id=undefined, channel=Ch},
    Res = west_protocol_handler:handle_event(subscribe, MsgSpec, WS),
    {_, ?MSG{event=Event}} = Res,
    BinRes = <<(<<"west ">>)/binary,
               (iolist_to_binary(Ch ++ ":"))/binary,
               (iolist_to_binary(Event))/binary>>,
    {ok, BinRes};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle the unsubscribe event.
%%
%% @end
%%--------------------------------------------------------------------
handle_event(["unsub", Ch], WS) ->
    MsgSpec = ?MSG{id=undefined, channel=Ch},
    Res = west_protocol_handler:handle_event(unsubscribe, MsgSpec, WS),
    {_, ?MSG{event=Event}} = Res,
    BinRes = <<(<<"west ">>)/binary,
               (iolist_to_binary(Ch ++ ":"))/binary,
               (iolist_to_binary(Event))/binary>>,
    {ok, BinRes};

handle_event(Any, _State) ->
    {none, Any}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Parse the text-based event.
%%
%% @end
%%--------------------------------------------------------------------
parse_msg(Msg) ->
    L = [string:strip(X, both, $ ) ||
         X <- string:tokens(binary_to_list(Msg), "\"")],
    case L of
        [C, M] -> string:tokens(C, " ") ++ [M];
        [C]    -> string:tokens(C, " ");
        _      -> none
    end.

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
ev_callback({ETag, Event, Msg}, [WSRef, _Id]) ->
    Body = case Msg of
               Msg when is_binary(Msg) ->
                   binary_to_list(Msg);
               _ ->
                   Msg
           end,
    Reply = <<(iolist_to_binary(ETag))/binary,
              (<<" ">>)/binary,
              (atom_to_binary(Event, utf8))/binary,
              (iolist_to_binary(":new_message "))/binary,
              (iolist_to_binary(Body))/binary>>,
    WSRef ! {event, Reply}.

