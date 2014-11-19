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
%%% @doc Interface into the WEST distributed application.
%%% @see <a href="http://basho.com/where-to-start-with-riak-core"></a>
%%% @end
%%% Created : 04. Nov 2013 8:47 PM
%%%-------------------------------------------------------------------
-module(west_dist).

%% API
-export([ping/0, cmd/3, cmd/4]).

%% Debug
-export([get_dbg_preflist/2, get_dbg_preflist/3]).

-include("west.hrl").

-define(TIMEOUT, 5000).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @spec ping() -> term()
%%
%% @doc
%% Pings a random vnode to make sure communication is functional.
%%
%% @end
%%--------------------------------------------------------------------
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, west),
    [{IdxNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IdxNode,
                                              ping,
                                              west_dist_vnode_master).

%%--------------------------------------------------------------------
%% @spec cmd(Bucket, Key, Val) ->
%%           {Res, ReqID} | {Res, ReqID, Reason} | {error, timeout}
%%
%% Types:
%%    Bucket = binary()
%%    Key = binary()
%%    Val = {Ref, Key, CbSpec}
%%
%% @equiv cmd(Bucket, Key, Val, [])
%%
%% @doc
%% Executes the command given in the `Val' spec.
%%
%% Bucket: Bucket to calculate hash (Riak Core).
%% Key: Key to calculate hash (Riak Core).
%% Val: Value that will be received by VNode.
%%
%% @see `west_dist_vnode'
%%
%% @end
%%--------------------------------------------------------------------
cmd(Bucket, Key, Val) ->
    cmd(Bucket, Key, Val, []).

%%--------------------------------------------------------------------
%% @spec cmd(Bucket, Key, Val, Opts) ->
%%           {Res, ReqID} | {Res, ReqID, Reason} | {error, timeout}
%%
%% Types:
%%    Bucket = binary()
%%    Key = binary()
%%    Val = {Ref, Key, CbSpec}
%%    Opts = proplist()
%%
%% @doc
%% Same as previous but it can receive option list.
%%
%% Bucket: Bucket to calculate hash (Riak Core).
%% Key: Key to calculate hash (Riak Core).
%% Val: Value that will be received by VNode.
%% Opts: Option list.
%%    q = quorum
%%    n = replicas
%%    Example: [{q, 1}, {n, 1}]
%%
%% @see `west_dist_vnode'
%%
%% @end
%%--------------------------------------------------------------------
cmd(Bucket, Key, Val, Opts) ->
    do_write(Bucket, Key, cmd, Val, Opts).

%%--------------------------------------------------------------------
%% @spec get_dbg_preflist(Bucket, Key) -> term()
%%
%% Types:
%%    Bucket = binary()
%%    Key = binary()
%%
%% @doc
%% Gets the preflist with default number of nodes (replicas).
%%
%% Bucket: Bucket to calculate hash (Riak Core).
%% Key: Key to calculate hash (Riak Core).
%%
%% @see `west_dist_vnode'
%%
%% @end
%%--------------------------------------------------------------------
get_dbg_preflist(Bucket, Key) ->
    get_dbg_preflist(Bucket, Key, ?N).

%%--------------------------------------------------------------------
%% @spec get_dbg_preflist(Bucket, Key, N) -> term()
%%
%% Types:
%%    Bucket = binary()
%%    Key = binary()
%%    N = non_neg_integer()
%%
%% @doc
%% Same as previous but it can receive the number of replicas (nodes).
%%
%% Bucket: Bucket to calculate hash (Riak Core).
%% Key: Key to calculate hash (Riak Core).
%% N: Number of replicas.
%%
%% @end
%%--------------------------------------------------------------------
get_dbg_preflist(Bucket, Key, N) ->
    DocIdx = riak_core_util:chash_key({iolist_to_binary(Bucket),
                                       iolist_to_binary(Key)}),
    riak_core_apl:get_apl(DocIdx, N, west).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Execute the command against the FSM.
%%
%% @end
%%--------------------------------------------------------------------
do_write(Bucket, Key, Op, Val, Opts) ->
    BBucket = iolist_to_binary(Bucket),
    BKey = iolist_to_binary(Key),
    {ok, ReqID} = west_dist_cmd_fsm:cmd(BBucket, BKey, Op, Val, Opts),
    wait_for_reqid(ReqID, ?TIMEOUT).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Waits for the FMS response.
%%
%% @end
%%--------------------------------------------------------------------
wait_for_reqid(ReqID, Timeout) ->
    receive
        {Code, ReqID} ->
            Code;
        {_Code, ReqID, Reply} ->
            case is_list(Reply) of
                true ->
                    case lists:keyfind(ok, 1, Reply) of
                        {_, V} -> V;
                        _      -> lists:last(Reply)
                    end;
                _ ->
                    Reply
            end
    after Timeout ->
	    {error, timeout}
    end.
