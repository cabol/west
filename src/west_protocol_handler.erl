%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Carlos Andres Bolaños, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License", F); you may not use this file
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
%%% @doc Protocol handler. This module handles the incoming events.
%%% @end
%%% Created : 10. Nov 2013 8:59 AM
%%%-------------------------------------------------------------------
-module(west_protocol_handler).

%% API
-export([handle_event/3]).

-include("west.hrl").
-include("west_int.hrl").
-include("west_protocol.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle the register event.
%%
%% @end
%%--------------------------------------------------------------------
handle_event(register,
             ?MSG{id=Id, channel=Ch},
             ?WEST_SERVER{name=Name, scope=Sc, cb=Cbk, format=F}=WS) ->
    case Ch =/= undefined of
        true ->
            Ev = west_utils:iolist_to_atom(Ch),
            Val = {west_lib, reg, [Sc, {Name, node()}, Ev, Cbk]},
            case execute(WS, Ch, Ch, Val) of
                {error, _} ->
                    {error, ?RES_INTERNAL_ERROR(Id, Ch, "Internal error.", F)};
                {_, registration_succeeded, _} ->
                    {ok, ?RES_REG_OK(Id, Ch, "Reg ok.", F)};
                {_, registration_denied, _} ->
                    {ok, ?RES_REG_DENIED(Id, Ch, "Reg denied.", F)};
                {_, registration_already_exist, _} ->
                    {ok, ?RES_REG_ALREADY_EXIST(Id, Ch, "Reg exist.", F)};
                _ ->
                    {error, ?RES_REG_FAILED(Id, Ch, "Reg failed.", F)}
            end;
        _ ->
            {error, ?RES_REG_FAILED(Id, Ch, "Channel undefined.", F)}
    end;

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle the unregister event.
%%
%% @end
%%--------------------------------------------------------------------
handle_event(unregister,
             ?MSG{id=Id, channel=Ch},
             ?WEST_SERVER{name=Name, format=F}=WS) ->
    case Ch =/= undefined of
        true ->
            Ev = west_utils:iolist_to_atom(Ch),
            Val = {west_lib, unreg, [{Name, node()}, Ev]},
            case execute(WS, Ch, Ch, Val) of
                {error, _} ->
                    {error, ?RES_INTERNAL_ERROR(Id, Ch, "Internal error.", F)};
                {_, unregistration_succeeded, _} ->
                    {ok, ?RES_UNREG_OK(Id, Ch, "Unreg ok.", F)};
                {_, registration_not_found, _} ->
                    {ok, ?RES_REG_NOT_FOUND(Id, Ch, "Reg not found", F)};
                _ ->
                    {error, ?RES_UNREG_FAILED(Id, Ch, "Unreg failed.", F)}
            end;
        _ ->
            {error, ?RES_UNREG_FAILED(Id, Ch, "Channel undefined.", F)}
    end;

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle the send event.
%%
%% @end
%%--------------------------------------------------------------------
handle_event(send,
             ?MSG{id=Id, channel=Ch, data=Data},
             ?WEST_SERVER{key=K, scope=Scope, format=F}=WS) ->
    case Ch =/= undefined andalso Data =/= undefined of
        true ->
            Ev = west_utils:iolist_to_atom(Ch),
            Val = {west_lib, send, [Scope, K, Ev, Data]},
            case execute(WS, Ch, Ch, Val) of
                {error, _} ->
                    {error, ?RES_INTERNAL_ERROR(Id, Ch, "Internal error.", F)};
                {_, sending_succeeded, _} ->
                    {ok, ?RES_SEND_OK(Id, Ch, "Message sent.", F)};
                {_, sending_failed, _} ->
                    {ok, ?RES_REG_NOT_FOUND(Id, Ch, "Reg not found", F)};
                _ ->
                    {error, ?RES_SEND_FAILED(Id, Ch, "Send failed.", F)}
            end;
        _ ->
            {error, ?RES_SEND_FAILED(Id, Ch, "Chann. or Msg is undefined.", F)}
    end;

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle the publish event.
%%
%% @end
%%--------------------------------------------------------------------
handle_event(publish,
             ?MSG{id=Id, channel=Ch, data=Data},
             ?WEST_SERVER{key=K, dist=Dist, format=F}) ->
    case Ch =/= undefined andalso Data =/= undefined of
        true ->
            Event = west_utils:iolist_to_atom(Ch),
            case Dist of
                gproc_dist ->
                    ?PS_PUB(g, K, Event, Data);
                _ ->
                    ?PS_PUB_ALL(l, K, Event, Data)
            end,
            {ok, ?RES_PUB_OK(Id, Ch, "Message published.", F)};
        _ ->
            {error, ?RES_PUB_FAILED(Id, Ch, "Chann. or Msg is undefined.", F)}
    end;

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle the subscribe event.
%%
%% @end
%%--------------------------------------------------------------------
handle_event(subscribe,
             ?MSG{id=Id, channel=Ch},
             ?WEST_SERVER{name=Name, key=K, scope=Sc, cb=Cbk, format=F}=WS) ->
    case Ch =/= undefined of
        true ->
            Ev = west_utils:iolist_to_atom(Ch),
            Val = {west_lib, sub, [Sc, {Name, node()}, Ev, Cbk]},
            case execute(WS, K, Ch, Val) of
                {error, _} ->
                    {error, ?RES_INTERNAL_ERROR(Id, Ch, "Internal error.", F)};
                {_, subscription_succeeded, _} ->
                    {ok, ?RES_SUB_OK(Id, Ch, "Subscription ok.", F)};
                {_, subscription_failed, _} ->
                    {ok, ?RES_SUB_FAILED(Id, Ch, "Sub error.", F)};
                {_, subscription_already_exist, _} ->
                    {ok, ?RES_SUB_ALREADY_EXIST(Id, Ch, "Sub exist.", F)};
                _ ->
                    {error, ?RES_SUB_FAILED(Id, Ch, "Subscription failed.", F)}
            end;
        _ ->
            {error, ?RES_SUB_FAILED(Id, Ch, "Channel undefined.", F)}
    end;

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle the unsubscribe event.
%%
%% @end
%%--------------------------------------------------------------------
handle_event(unsubscribe,
             ?MSG{id=Id, channel=Ch},
             ?WEST_SERVER{name=Name, key=K, format=F}=WS) ->
    case Ch =/= undefined of
        true ->
            Ev = west_utils:iolist_to_atom(Ch),
            Val = {west_lib, unsub, [{Name, node()}, Ev]},
            case execute(WS, K, Ch, Val) of
                {error, _} ->
                    {error, ?RES_INTERNAL_ERROR(Id, Ch, "Internal error.", F)};
                {_, unsubscription_succeeded, _} ->
                    {ok, ?RES_UNSUB_OK(Id, Ch, "Unsub ok.", F)};
                {_, subscription_not_found, _} ->
                    {ok, ?RES_SUB_NOT_FOUND(Id, Ch, "Sub not found.", F)};
                _ ->
                    {error, ?RES_UNSUB_FAILED(Id, Ch, "Unsub failed.", F)}
            end;
        _ ->
            {error, ?RES_UNSUB_FAILED(Id, Ch, "Channel undefined.", F)}
    end;

handle_event(Any, _Msg, _State) ->
    {none, Any}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Executes the asked command.
%%
%% @end
%%--------------------------------------------------------------------
execute(?WEST_SERVER{dist=Dist, west_dist=WDist}, B, K, {M, F, A}=Val) ->
    case Dist of
        gproc_dist ->
            apply(M, F, A);
        _ ->
            Opts = proplists:get_value(opts, WDist, []),
            apply(west_dist, cmd, [B, K, Val, Opts])
    end.
