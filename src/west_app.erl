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
%%% @doc This is the application module of `west'.
%%% @end
%%% Created : 03. Oct 2013 9:57 AM
%%%-------------------------------------------------------------------
-module(west_app).

-behaviour(application).

%% application
-export([start/0, start/2, stop/1]).

-define(C_ACCEPTORS,  100).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    start(normal, []).

start(_Type, _Args) ->
    case west_sup:start_link() of
        {ok, Sup} ->
            start_riak_core(),
            start_web_server(),
            {ok, Sup};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_riak_core() ->
    case application:get_env(west, dist) of
        {ok, west_dist} ->
            west_utils:start_app_deps(riak_core),
            ok = riak_core:register(west, [{vnode_module, west_dist_vnode}]),
            %%ok = riak_core_ring_events:add_guarded_handler(west_ring_event_handler, []),
            %%ok = riak_core_node_watcher_events:add_guarded_handler(west_node_event_handler, []),
            ok = riak_core_node_watcher:service_up(west, self());
        _ ->
            ok
    end.

start_web_server() ->
    case application:get_env(west, web_server) of
        {ok, yaws} ->
            start_yaws();
        {ok, cowboy} ->
            start_cowboy();
        _ ->
            ok
    end.

start_yaws() ->
    case application:get_all_env(yaws) of
        [] ->
            ok;
        Yaws ->
            Id      = "embedded",
            Gconf   = proplists:get_value(gconf, Yaws, gconf()),
            Sconf   = proplists:get_value(sconf, Yaws, sconf()),
            Docroot = proplists:get_value(docroot, Sconf, "./www"),
            ok      = yaws:start_embedded(Docroot, Sconf, Gconf, Id)
    end.

gconf() ->
    [{id, "embedded"}, {ebin_dir, ["./ebin"]}, {runmod, "yapp"}].

sconf() ->
    [{servername, "west_server"},
     {listen, {127,0,0,1}},
     {port, 8080},
     {docroot, "./www"},
     {appmods, [{"websocket", west_ws_endpoint}]},
     {opaque, [{yapp_server_id, "yapp_west"},
               {bootstrap_yapps, "west"}]}].

start_cowboy() ->
    case application:get_all_env(cowboy) of
        [] ->
            ok;
        _ ->
            west_utils:start_app_deps(cowboy),
            Routes     = application:get_env(cowboy, routes, cowboy_routes()),
            Dispatch   = cowboy_router:compile(Routes),
            TransOpts  = application:get_env(cowboy, trans_opts, [{port, 8080}]),
            ProtoOpts  = [{env, [{dispatch, Dispatch}]}],
            Cacceptors = application:get_env(cowboy, c_acceptors, ?C_ACCEPTORS),
            {ok, _}    = cowboy:start_http(http, Cacceptors, TransOpts, ProtoOpts)
    end.

cowboy_routes() ->
    [
     {'_', [
            {"/", cowboy_static, {file, "./www/index.html"}},
            {"/websocket/text/:key", west_ws_text_protocol_handler, []},
            {"/websocket/json/:key", west_ws_json_protocol_handler, []},
            {"/websocket/pb/:key", west_ws_pb_protocol_handler, []},
            {"/[...]", cowboy_static, {dir, "./www", [{mimetypes, cow_mimetypes, all}]}}
           ]
     }
    ].
