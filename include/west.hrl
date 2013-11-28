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
%%% @doc Common definitions.
%%% @end
%%% Created : 03. Oct 2013 9:57 AM
%%%-------------------------------------------------------------------

-record(eh_cb_spec, {mod, func, args}).
-define(CALLBACK_SPEC, #eh_cb_spec).

-record(msg_data_spec, {channel, channel_data, body}).
-define(MSG_DATA, #msg_data_spec).

-record(msg_spec, {id, from, event, data=?MSG_DATA{}}).
-define(MSG, #msg_spec).

-define(MSG_FIELDS, [id, from, event, data]).
-define(MSG_DATA_FIELDS, [channel, channel_data, body]).

-record(channel_spec, {name, type, user_id, date}).
-define(CHANNEL, #channel_spec).

-record(west_server_spec, {name, key, dist, west_dist, scope, cb, format}).
-define(WEST_SERVER, #west_server_spec).

-type scope()       :: l | g.
-type event()       :: any().
-type msg()         :: any().
-type key()         :: iolist() | atom().
-type bucket()      :: iolist() | atom().
-type cb_spec()     :: {module(), function() | atom(), [term()] | undefined}.
-type msg_spec()    :: msg_spec().

-type name()        :: atom().
-type gname()       :: term().
-type server_ref()  :: name() | {name(), node()} | {global, gname()} | pid().
-type property()    :: {atom(), term()}.
-type proplist()    :: [property()].

-define(PRINT(Var),
    io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).

-define(LOG_INFO(Format, Vars),
    error_logger:info_msg(Format, Vars)).

-define(N, 3).
-define(R, 2).
-define(W, 2).


