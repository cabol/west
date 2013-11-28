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
%%% @doc Supervisor of 'west_event_handler'.
%%% @end
%%% Created : 03. Oct 2013 9:57 AM
%%%-------------------------------------------------------------------
-module(west_event_handler_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/3]).

%% Callback
-export([init/1]).

-include("west.hrl").

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Start a gen_server to register the subscription and handle incoming
%% events to this subscription.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%% Starts a new child `west_event_handler'.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_child(scope(), cb_spec(), proplist()) ->
                  {ok, pid()} | {error, term()}.
start_child(Scope, CallbackSpec, Opts) ->
    supervisor:start_child(?SERVER, [Scope, CallbackSpec, Opts]).

%%%===================================================================
%%% Supervisor callbacks
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
init(_Args) ->
    Element = {west_event_handler,
               {west_event_handler, start_link, []},
               transient,
               brutal_kill,
               worker,
               [west_event_handler]},
    Children = [Element],
    RestartStrategy = {simple_one_for_one, 10, 60},
    {ok, {RestartStrategy, Children}}.
