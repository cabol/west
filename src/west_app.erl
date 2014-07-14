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

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    start(normal, []).

start(_Type, _Args) ->
    %% Get YAWS properties, if exist
    Yaws = case init:get_argument(config) of
               {ok, [[ConfigFile]]} ->
                   case file:consult(ConfigFile) of
                       {ok, [Config]} ->
                           proplists:get_value(yaws, Config);
                       _ ->
                           undefined
                   end;
               _ ->
                   undefined
           end,
    %% Start WEST
    case west_sup:start_link(Yaws) of
        {ok, Sup} ->
            start_riak_core(),
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
        {ok, gproc_dist} ->
            ok;
        _ ->
            ok = riak_core:register(west, [{vnode_module, west_dist_vnode}]),
            %%ok = riak_core_ring_events:add_guarded_handler(west_ring_event_handler, []),
            %%ok = riak_core_node_watcher_events:add_guarded_handler(west_node_event_handler, []),
            ok = riak_core_node_watcher:service_up(west, self())
    end.
