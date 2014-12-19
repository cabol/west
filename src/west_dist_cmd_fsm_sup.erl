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
%%% @doc Supervise the west_dist_cmd FSM.
%%% @see <a href="http://basho.com/where-to-start-with-riak-core"></a>
%%% @end
%%% Created : 04. Nov 2013 8:47 PM
%%%-------------------------------------------------------------------
-module(west_dist_cmd_fsm_sup).

-behavior(supervisor).

%% API
-export([start_cmd_fsm/1,
         start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts a new child (worker).
%% @spec start_cmd_fsm(Args :: list()) -> supervisor:startchild_ret()
start_cmd_fsm(Args) ->
    supervisor:start_child(?MODULE, Args).

%% @doc Starts the supervisor
%% @spec start_link() -> supervisor:startlink_ret()
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
init([]) ->
    CmdFsm = {west_dist_cmd_fsm,
              {west_dist_cmd_fsm, start_link, []},
              temporary,
              5000,
              worker,
              [west_dist_cmd_fsm]},
    {ok, {{simple_one_for_one, 10, 10}, [CmdFsm]}}.
