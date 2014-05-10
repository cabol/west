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
%%% @doc `WEST' supervisor.
%%% @end
%%% Created : 03. Oct 2013 9:57 AM
%%%-------------------------------------------------------------------
-module(west_sup).

-behaviour(supervisor).

%% Public API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @spec start_link(Args :: list()) -> {ok, pid()} |
%%                                     ignore |
%%                                     {error, Reason :: term()}
%%
%% @doc
%% Starts the `WEST' supervisor.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(list()) -> {ok, pid()} | {error, term()}.
start_link(Args) when is_list(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Args]).

%%%===================================================================
%%% Supervisor Callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
init(Args) ->
    {ok, {{one_for_one, 5, 10}, process_specs(Args)}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Build the process specifications that will be supervised.
%%
%% @end
%%--------------------------------------------------------------------
-spec process_specs(list()) -> [supervisor:child_spec()].
process_specs(Args) ->
    EvHdlr_sup = {west_event_handler_sup,
                  {west_event_handler_sup, start_link, []},
                  permanent,
                  2000,
                  supervisor,
                  [west_event_handler_sup]},
    Dist = case application:get_env(west, dist) of
               {ok, gproc_dist} ->
                   [];
               _ ->
                   VMaster = {west_dist_vnode_master,
                              {riak_core_vnode_master,
                               start_link,
                               [west_dist_vnode]},
                              permanent,
                              5000,
                              worker,
                              [riak_core_vnode_master]},
                   CmdFSM = {west_dist_cmd_fsm_sup,
                             {west_dist_cmd_fsm_sup, start_link, []},
                             permanent,
                             infinity,
                             supervisor,
                             [west_dist_cmd_fsm_sup]},
                   [VMaster, CmdFSM]
           end,
    Yaws = case Args of
               undefined ->
                   [];
               _ when is_list(Args) ->
                   Ybed_sup = {ybed_sup,
                               {ybed_sup, start_link, Args},
                               permanent,
                               2000,
                               supervisor,
                               [ybed_sup]},
                   [Ybed_sup]
           end,
    [EvHdlr_sup] ++ Dist ++ Yaws.

