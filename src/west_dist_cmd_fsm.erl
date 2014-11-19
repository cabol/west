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
-export([start_link/5,
         start_link/7,
         cmd/3,
         cmd/5]).

%% Callbacks
-export([init/1,
         code_change/4,
         handle_event/3,
         handle_info/3,
         handle_sync_event/4,
         terminate/3]).

%% States
-export([prepare/2,
         execute/2,
         waiting/2]).

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

%%--------------------------------------------------------------------
%% @spec start_link(ReqID, From, Bucket, Key, Op) ->
%%                  {ok, pid()} | ignore | {error, Reason :: term()}
%%
%% Types:
%%    ReqID = pos_integer()
%%    From = pid()
%%    Bucket = binary()
%%    Key = binary()
%%    Op = atom()
%%
%% @equiv start_link(ReqID, From, Bucket, Key, Op, undefined, [])
%%
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% ReqID: The request id so the caller can verify the response.
%% From: The pid of the sender so a reply can be made.
%% Bucket: Bucket to calculate hash.
%% Key: Key to calculate hash.
%% Op: The cmd op, one of [reg, unreg, send, sub, unsub, pub]
%%
%% @end
%%--------------------------------------------------------------------
start_link(ReqID, From, Bucket, Key, Op) ->
    start_link(ReqID, From, Bucket, Key, Op, undefined, []).

%%--------------------------------------------------------------------
%% @spec start_link(ReqID, From, Bucket, Key, Op, Val, Opts) ->
%%                  {ok, pid()} | ignore | {error, Reason :: term()}
%%
%% Types:
%%    ReqID = pos_integer()
%%    From = pid()
%%    Bucket = binary()
%%    Key = binary()
%%    Op = atom()
%%    Val = any()
%%    Opts = proplist()
%%
%% @doc
%% Same of the previous function but, it can receive value and
%% option list.
%%
%% ReqID: The request id so the caller can verify the response.
%% From: The pid of the sender so a reply can be made.
%% Bucket: Bucket to calculate hash.
%% Key: Key to calculate hash.
%% Op: The cmd op, one of [reg, unreg, send, sub, unsub, pub]
%% Val: Any value that is passed to FSM.
%% Opts: Option list.
%%    q = quorum
%%    n = replicas
%%    Example: [{q, 1}, {n, 1}]
%%
%% @end
%%--------------------------------------------------------------------
start_link(ReqID, From, Bucket, Key, Op, Val, Opts) ->
    gen_fsm:start_link(?MODULE, [ReqID, From, Bucket, Key, Op, Val, Opts], []).

%%--------------------------------------------------------------------
%% @spec cmd(Bucket, Key, Op) ->
%%           {ok, pid()} | ignore | {error, Reason :: term()}
%%
%% Types:
%%    Bucket = binary()
%%    Key = binary()
%%    Op = atom()
%%
%% @equiv cmd(Bucket, Key, Op, undefined, [])
%%
%% @doc
%% Start the FSM in order to execute the given command.
%%
%% Bucket: Bucket to calculate hash.
%% Key: Key to calculate hash.
%% Op: The cmd op, one of [reg, unreg, send, sub, unsub, pub]
%%
%% @end
%%--------------------------------------------------------------------
cmd(Bucket, Key, Op) ->
    cmd(Bucket, Key, Op, undefined, []).

%%--------------------------------------------------------------------
%% @spec cmd(Bucket, Key, Op) ->
%%           {ok, pid()} | ignore | {error, Reason :: term()}
%%
%% Types:
%%    Bucket = binary()
%%    Key = binary()
%%    Op = atom()
%%    Val = any()
%%    Opts = proplist()
%%
%% @doc
%% Same of previous but with value and option list.
%%
%% Bucket: Bucket to calculate hash.
%% Key: Key to calculate hash.
%% Op: The cmd op, one of [reg, unreg, send, sub, unsub, pub]
%% Val: Any value that is passed to FSM.
%% Opts: Option list.
%%    q = quorum
%%    n = replicas
%%    Example: [{q, 1}, {n, 1}]
%%
%% @end
%%--------------------------------------------------------------------
cmd(Bucket, Key, Op, Val, Opts) ->
    ReqID = mk_reqid(),
    west_dist_cmd_fsm_sup:start_cmd_fsm([ReqID,
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @end
%%--------------------------------------------------------------------
init([ReqID, From, Bucket, Key, Op, Val, Opts]) ->
    Q = west_utils:keyfind(q, Opts, ?W),
    N = west_utils:keyfind(n, Opts, ?N),
    SD = #state{req_id=ReqID,
                from=From,
                bkey={Bucket, Key},
                op=Op,
                val=Val,
                n=N,
                q=Q},
    {ok, prepare, SD, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Prepare the write by calculating the _preference list_.
%%
%% @end
%%--------------------------------------------------------------------
prepare(timeout, SD0=#state{bkey=BKey, n=N}) ->
    DocIdx = riak_core_util:chash_key(BKey),
    PrefList = riak_core_apl:get_apl(DocIdx, N, west),
    SD = SD0#state{preflist=PrefList},
    {next_state, execute, SD, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Execute the command request and then go into waiting state to
%% verify it has meets consistency requirements.
%%
%% @end
%%--------------------------------------------------------------------
execute(timeout,
        SD0=#state{req_id=ReqID, op=Op, val=Val, preflist=PrefList}) ->
    case Val of
        undefined ->
            west_dist_vnode:Op(PrefList, ReqID);
        _ ->
            west_dist_vnode:Op(PrefList, ReqID, Val)
    end,
    {next_state, waiting, SD0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Wait for Q write reqs to respond.
%%
%% @end
%%--------------------------------------------------------------------
waiting({_Res, ReqID, Val},
        SD0=#state{from=From, num_q=NumQ0, q=Q, replies=Replies0}) ->
    NumQ = NumQ0 + 1,
    Replies = [Val|Replies0],
    SD = SD0#state{num_q=NumQ, replies=Replies},
    if
        NumQ =:= Q ->
            Reply = case lists:any(different(Val), Replies) of
                        true  -> Replies;
                        false -> Val
                    end,
            From ! {ok, ReqID, Reply},
            {stop, normal, SD};
        true -> {next_state, waiting, SD}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If any event info is received, FSM finish.
%%
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, _StateName, StateData) ->
    {stop, badmsg, StateData}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If any event is received, FSM finish.
%%
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, _StateName, StateData) ->
    {stop, badmsg, StateData}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% If any event is received, FSM finish.
%%
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop, badmsg, StateData}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _SN, _SD) ->
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Calculates a random request id.
%%
%% @end
%%--------------------------------------------------------------------
mk_reqid() -> erlang:phash2(os:timestamp()).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Difference.
%%
%% @end
%%--------------------------------------------------------------------
different(A) -> fun(B) -> A =/= B end.
