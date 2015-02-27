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
%%% @doc The coordinator for command opeartions.
%%% @see <a href="http://basho.com/where-to-start-with-riak-core"></a>
%%% @end
%%% Created : 04. Nov 2013 8:47 PM
%%%-------------------------------------------------------------------
-module(west_dist_cmd_fsm).

-behavior(gen_fsm).

-include("west.hrl").

%% API
-export([start_link/5, start_link/7,
         cmd/3, cmd/5]).

%% Callbacks
-export([init/1,
         code_change/4,
         handle_event/3,
         handle_info/3,
         handle_sync_event/4,
         terminate/3]).

%% States
-export([prepare/2, execute/2, waiting/2]).

%% State
-record(state, {req_id :: pos_integer(),
                from :: pid(),
                bkey :: binary(),
                op :: atom(),
                val = undefined :: term() | undefined,
                preflist :: riak_core_apl:preflist2(),
                n :: non_neg_integer(),
                num_q = 0 :: non_neg_integer(),
                q = ?W :: non_neg_integer(),
                replies = [] :: list()}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc start_link/5.
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%% <br/>
%% <li>ReqID: The request id so the caller can verify the response.</li>
%% <li>From: The pid of the sender so a reply can be made.</li>
%% <li>Bucket: Bucket to calculate hash.</li>
%% <li>Key: Key to calculate hash.</li>
%% <li>Op: The cmd op, one of <code>reg|unreg|send|sub|unsub|pub</code></li>
%%
%% @spec start_link(ReqID, From, Bucket, Key, Op) ->
%%                  {ok, pid()} | ignore | {error, Reason :: term()}
%% ReqID = pos_integer()
%% From = pid()
%% Bucket = binary()
%% Key = binary()
%% Op = atom()
%%
%% @equiv start_link(ReqID, From, Bucket, Key, Op, undefined, [])
start_link(ReqID, From, Bucket, Key, Op) ->
  start_link(ReqID, From, Bucket, Key, Op, undefined, []).

%% @doc start_link/7.
%% Same of the previous function but, it can receive value and
%% option list.
%% <br/>
%% <li>ReqID: The request id so the caller can verify the response.</li>
%% <li>From: The pid of the sender so a reply can be made.</li>
%% <li>Bucket: Bucket to calculate hash.</li>
%% <li>Key: Key to calculate hash.</li>
%% <li>Op: The cmd op, one of <code>reg|unreg|send|sub|unsub|pub</code></li>
%% <li>Val: Any value that is passed to FSM.</li>
%% <li>
%% Opts: Option list.
%% q = quorum
%% n = replicas
%% Example: <code>[{q, 1}, {n, 1}]</code>
%% </li>
%%
%% @spec start_link(ReqID, From, Bucket, Key, Op, Val, Opts) ->
%%                  {ok, pid()} | ignore | {error, Reason :: term()}
%% ReqID = pos_integer()
%% From = pid()
%% Bucket = binary()
%% Key = binary()
%% Op = atom()
%% Val = any()
%% Opts = proplist()
start_link(ReqID, From, Bucket, Key, Op, Val, Opts) ->
  gen_fsm:start_link(?MODULE, [ReqID, From, Bucket, Key, Op, Val, Opts], []).

%% @doc cmd/3.
%% Start the FSM in order to execute the given command.
%% <li>Bucket: Bucket to calculate hash.</li>
%% <li>Key: Key to calculate hash.</li>
%% <li>Op: The cmd op, one of <code>reg|unreg|send|sub|unsub|pub</code></li>
%%
%% @spec cmd(Bucket, Key, Op) ->
%%           {ok, pid()} | ignore | {error, Reason :: term()}
%% Bucket = binary()
%% Key = binary()
%% Op = atom()
%%
%% @equiv cmd(Bucket, Key, Op, undefined, [])
cmd(Bucket, Key, Op) ->
  cmd(Bucket, Key, Op, undefined, []).

%% @doc cmd/5.
%% Same of previous but with value and option list.
%% <li>Bucket: Bucket to calculate hash.</li>
%% <li>Key: Key to calculate hash.</li>
%% <li>Op: The cmd op, one of <code>reg|unreg|send|sub|unsub|pub</code></li>
%% <li>Val: Any value that is passed to FSM.</li>
%% <li>
%% Opts: Option list.
%% q = quorum
%% n = replicas
%% Example: <code>[{q, 1}, {n, 1}]</code>
%% </li>
%%
%% @spec cmd(Bucket, Key, Op, Val, Opts) ->
%%           {ok, pid()} | ignore | {error, Reason :: term()}
%% Bucket = binary()
%% Key = binary()
%% Op = atom()
%% Val = any()
%% Opts = proplist()
cmd(Bucket, Key, Op, Val, Opts) ->
  ReqID = mk_reqid(),
  west_dist_cmd_fsm_sup:start_cmd_fsm(
    [ReqID,
    self(),
    Bucket,
    Key,
    Op,
    Val,
    Opts]),
  {ok, ReqID}.

%%%===================================================================
%%% States
%%%===================================================================

%% @private
init([ReqID, From, Bucket, Key, Op, Val, Opts]) ->
  Q = west_utils:keyfind(q, Opts, ?W),
  N = west_utils:keyfind(n, Opts, ?N),
  SD = #state{req_id = ReqID,
              from = From,
              bkey = {Bucket, Key},
              op = Op,
              val = Val,
              n = N,
              q = Q},
  {ok, prepare, SD, 0}.

%% @private
%% @doc Prepare the write by calculating the _preference list_.
prepare(timeout, SD0 = #state{bkey = BKey, n = N}) ->
  DocIdx = riak_core_util:chash_key(BKey),
  PrefList = riak_core_apl:get_apl(DocIdx, N, west),
  SD = SD0#state{preflist = PrefList},
  {next_state, execute, SD, 0}.

%% @private
%% @doc Execute the command request and then go into waiting state to
%%      verify it has meets consistency requirements.
execute(timeout,
        SD0 = #state{req_id = ReqID, op = Op, val = Val, preflist = PrefL}) ->
  case Val of
    undefined ->
      west_dist_vnode:Op(PrefL, ReqID);
    _ ->
      west_dist_vnode:Op(PrefL, ReqID, Val)
  end,
  {next_state, waiting, SD0}.

%% @private
%% @doc Wait for Q write reqs to respond.
waiting({_Res, ReqID, Val},
        SD0 = #state{from = From, num_q = NumQ0, q = Q, replies = Replies0}) ->
  NumQ = NumQ0 + 1,
  Replies = [Val | Replies0],
  SD = SD0#state{num_q = NumQ, replies = Replies},
  case NumQ =:= Q of
    true ->
      Reply = case lists:any(different(Val), Replies) of
                true -> Replies;
                false -> Val
              end,
      From ! {ok, ReqID, Reply},
      {stop, normal, SD};
    false ->
      {next_state, waiting, SD}
  end.

%% @private
%% @doc If any event info is received, FSM finish.
handle_info(_Info, _StateName, StateData) ->
  {stop, badmsg, StateData}.

%% @private
%% @doc If any event is received, FSM finish.
handle_event(_Event, _StateName, StateData) ->
  {stop, badmsg, StateData}.

%% @private
%% @doc If any event is received, FSM finish.
handle_sync_event(_Event, _From, _StateName, StateData) ->
  {stop, badmsg, StateData}.

%% @private
%% @doc Convert process state when code is changed.
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%% @private
terminate(_Reason, _SN, _SD) ->
  ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private
mk_reqid() -> erlang:phash2(os:timestamp()).

%% @private
different(A) -> fun(B) -> A =/= B end.
