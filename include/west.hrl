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
%%% @doc Common definitions.
%%% @end
%%% Created : 03. Oct 2013 9:57 AM
%%%-------------------------------------------------------------------

-record(callback_t, {mod, func, args}).
-define(CALLBACK, #callback_t).

-record(msg_t, {id, from, event, channel, data}).
-define(MSG, #msg_t).

-define(MSG_FIELDS, [id, from, event, channel, data]).

-record(channel_t, {name, type, user_id, date}).
-define(CHANNEL, #channel_t).

-record(west_t, {name, key, dist, dist_props, scope, cb, encoding}).
-define(WEST, #west_t).

-type scope()      :: l | g.
-type event()      :: any().
-type msg()        :: any().
-type key()        :: iolist() | atom().
-type bucket()     :: iolist() | atom().
-type cb_spec()    :: {module(), function() | atom(), [term()] | undefined}.
-type msg_spec()   :: #msg_t{}.
-type name()       :: atom().
-type gname()      :: term().
-type server_ref() :: name() | {name(), node()} | {global, gname()} | pid().
-type property()   :: {atom(), term()}.
-type proplist()   :: [property()].

-define(PRINT(Var),
  io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])
).

-define(LOG_INFO(Format, Vars),
  error_logger:info_msg(Format, Vars)
).
-define(LOG_ERROR(Format, Vars),
  error_logger:error_msg(Format, Vars)
).
-define(LOG_WARNING(Format, Vars),
  error_logger:warning_msg(Format, Vars)
).

-define(GPROC_SCOPE(X),
  case X of
    gproc_dist -> g;
    _ -> l
  end
).

-define(N, 3).
-define(R, 2).
-define(W, 2).

%% This macro will create a function that converts a record to
%% a {key, value} list (a proplist)
%% Taken from <a href="https://gist.github.com/gdamjan/1272771"/>
-define(record_to_list(Record),
  fun(Val) ->
    Fields = record_info(fields, Record),
    [_Tag | Values] = tuple_to_list(Val),
    lists:zip(Fields, Values)
  end
).
