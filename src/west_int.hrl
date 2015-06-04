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
%%% @doc Shared internal functions.
%%% @end
%%% Created : 03. Oct 2013 9:57 AM
%%%-------------------------------------------------------------------

-define(CATCH_ERROR(Expr),
  try Expr
  catch _:Error -> {error, Error}
  end
).

-define(THROW_ERROR(E), throw({error, E})).

-define(PS_SUB(Scope, Event),
  try
    gproc_ps:subscribe(Scope, Event)
  catch
    _:_ -> {error, gproc_sub_failed}
  end
).

-define(PS_SUB_EXT(Pid, Scope, Event),
  try
    GPKey = {p, Scope, {gproc_ps_event, Event}},
    case gproc_lib:insert_reg(GPKey, gproc:default(GPKey), Pid, Scope) of
      false ->
        {error, badarg};
      true ->
        erlang:monitor(process, Pid),
        true
    end
  catch
    _:_ -> {error, gproc_sub_failed}
  end
).

-define(PS_UNSUB(Scope, Event),
  try
    gproc_ps:unsubscribe(Scope, Event)
  catch
    _:_ -> {error, gproc_unsub_failed}
  end
).

-define(PS_UNSUB_EXT(Pid, Scope, Event),
  try
    GPKey = {p, Scope, {gproc_ps_event, Event}},
    gproc_lib:remove_reg(GPKey, Pid, unreg),
    true
  catch
    _:_ -> {error, gproc_unsub_failed}
  end
).

-define(PS_PUB(Scope, ETag, Event, Msg),
  try
    gproc:send({p, Scope, {gproc_ps_event, Event}}, {ETag, Event, Msg}),
    true
  catch
    _:_ -> {error, gproc_pub_failed}
  end
).

-define(PS_PUB_ALL(Scope, ETag, Event, Msg),
  try
    rpc:multicall(
      gproc, send, [{p, Scope, {gproc_ps_event, Event}}, {ETag, Event, Msg}]),
    true
  catch
    _:_ -> {error, gproc_pub_failed}
  end
).

-define(REG(Scope, Key),
  try
    gproc:reg({n, Scope, Key})
  catch
    _:_ -> {error, gproc_reg_failed}
  end
).

-define(REG_EXT(Pid, Scope, Key),
  try
    GPKey = {n, Scope, Key},
    case gproc_lib:insert_reg(GPKey, gproc:default(GPKey), Pid, Scope) of
      false ->
        {error, badarg};
      true ->
        erlang:monitor(process, Pid),
        true
    end
  catch
    _:_ -> {error, gproc_reg_failed}
  end
).

-define(UNREG(Scope, Key),
  try
    gproc:unreg({n, Scope, Key})
  catch
    _:_ -> {error, gproc_unreg_failed}
  end
).

-define(UNREG_EXT(Pid, Scope, Key),
  try
    GPKey = {n, Scope, Key},
    gproc_lib:remove_reg(GPKey, Pid, unreg),
    true
  catch
    _:_ -> {error, gproc_unreg_failed}
  end
).

-define(SEND(Scope, ETag, Key, Msg),
  try
    gproc:send({n, Scope, Key}, {ETag, Key, Msg}),
    true
  catch
    _:_ -> {error, gproc_send_failed}
  end
).

-define(WHERE(Scope, Key),
  try
    gproc:where({n, Scope, Key})
  catch
    _:_ -> {error, gproc_where_failed}
  end
).

-define(PROC_INFO(Pid),
  try
    {gproc, Value} = gproc:info(Pid, gproc),
    Value
  catch
    _:_ -> {error, gproc_info_failed}
  end
).

-define(PROC_TYPE(Pid),
  try
    {gproc, [{{Type, _, _}, _} | _T]} = gproc:info(Pid, gproc),
    Type
  catch
    _:_ -> {error, gproc_info_type_failed}
  end
).

-define(ENC_JSON(JsonTerm), west_util:enc_json(JsonTerm)).

-define(DEC_JSON(JsonData), west_util:dec_json(JsonData)).
