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
%%% @doc Protocol Macros.
%%% @end
%%% Created : 06. Oct 2013 8:45 AM
%%%-------------------------------------------------------------------

-define(EVENTS, [{0, connection_established},
                 {1, internal_error},
                 {2, message_received},
                 {3, action_not_allowed},
                 {4, channel_not_found},
                 {5, channel_creation_succeeded},
                 {6, channel_already_exist},
                 {7, channel_creation_failed},
                 {8, channel_delete_succeeded},
                 {9, channel_delete_failed},
                 {100, registration_succeeded},
                 {101, registration_failed},
                 {102, registration_already_exist},
                 {103, registration_not_found},
                 {104, registration_denied},
                 {200, unregistration_succeeded},
                 {201, unregistration_failed},
                 {300, sending_succeeded},
                 {301, sending_failed},
                 {400, subscription_succeeded},
                 {401, subscription_already_exist},
                 {402, subscription_not_found},
                 {403, subscription_failed},
                 {500, unsubscription_succeeded},
                 {501, unsubscription_failed},
                 {600, publication_succeeded},
                 {601, publication_failed}]).

-define(RES_CONN_ESTABLISHED(C, B, F),
    west_msg_utils:build_msg(
        undefined, "west", "west:connection_established", C, undefined, B, F)).

-define(RES_INTERNAL_ERROR(Id, C, B, F),
    west_msg_utils:build_msg(
        Id, "west", "west:internal_error", C, undefined, B, F)).

-define(RES_MSG_RECEIVED(Id, ET, C, B, F),
    west_msg_utils:build_msg(
        Id, ET, "west:message_received", C, undefined, B, F)).

-define(RES_ACTION_NOT_ALLOWED(Id, C, B, F),
    west_msg_utils:build_msg(
        Id, "west", "west:action_not_allowed", C, undefined, B, F)).

-define(RES_CH_NOT_FOUND(Id, C, B, F),
    west_msg_utils:build_msg(
        Id, "west", "west:channel_not_found", C, undefined, B, F)).

-define(RES_CH_CREATION_OK(Id, C, B, F),
    west_msg_utils:build_msg(
        Id, "west", "west:channel_creation_succeeded", C, undefined, B, F)).

-define(RES_CH_ALREADY_EXIST(Id, C, B, F),
    west_msg_utils:build_msg(
        Id, "west", "west:channel_already_exist", C, undefined, B, F)).

-define(RES_CH_CREATION_FAILED(Id, C, B, F),
    west_msg_utils:build_msg(
        Id, "west", "west:channel_creation_failed", C, undefined, B, F)).

-define(RES_CH_DELETE_OK(Id, C, B, F),
    west_msg_utils:build_msg(
        Id, "west", "west:channel_delete_succeeded", C, undefined, B, F)).

-define(RES_CH_DELETE_FAILED(Id, C, B, F),
    west_msg_utils:build_msg(
        Id, "west", "west:channel_delete_failed", C, undefined, B, F)).

-define(RES_REG_OK(Id, C, B, F),
    west_msg_utils:build_msg(
        Id, "west", "west:registration_succeeded", C, undefined, B, F)).

-define(RES_REG_FAILED(Id, C, B, F),
    west_msg_utils:build_msg(
        Id, "west", "west:registration_failed", C, undefined, B, F)).

-define(RES_REG_ALREADY_EXIST(Id, C, B, F),
    west_msg_utils:build_msg(
        Id, "west", "west:registration_already_exist", C, undefined, B, F)).

-define(RES_REG_NOT_FOUND(Id, C, B, F),
    west_msg_utils:build_msg(
        Id, "west", "west:registration_not_found", C, undefined, B, F)).

-define(RES_REG_DENIED(Id, C, B, F),
    west_msg_utils:build_msg(
        Id, "west", "west:registration_denied", C, undefined, B, F)).

-define(RES_UNREG_OK(Id, C, B, F),
    west_msg_utils:build_msg(
        Id, "west", "west:unregistration_succeeded", C, undefined, B, F)).

-define(RES_UNREG_FAILED(Id, C, B, F),
    west_msg_utils:build_msg(
        Id, "west", "west:unregistration_failed", C, undefined, B, F)).

-define(RES_SEND_OK(Id, C, B, F),
    west_msg_utils:build_msg(
        Id, "west", "west:sending_succeeded", C, undefined, B, F)).

-define(RES_SEND_FAILED(Id, C, B, F),
    west_msg_utils:build_msg(
        Id, "west", "west:sending_failed", C, undefined, B, F)).

-define(RES_SUB_OK(Id, C, B, F),
    west_msg_utils:build_msg(
        Id, "west", "west:subscription_succeeded", C, undefined, B, F)).

-define(RES_SUB_ALREADY_EXIST(Id, C, B, F),
    west_msg_utils:build_msg(
        Id, "west", "west:subscription_already_exist", C, undefined, B, F)).

-define(RES_SUB_NOT_FOUND(Id, C, B, F),
    west_msg_utils:build_msg(
        Id, "west", "west:subscription_not_found", C, undefined, B, F)).

-define(RES_SUB_FAILED(Id, C, B, F),
    west_msg_utils:build_msg(
        Id, "west", "west:subscription_failed", C, undefined, B, F)).

-define(RES_UNSUB_OK(Id, C, B, F),
    west_msg_utils:build_msg(
        Id, "west", "west:unsubscription_succeeded", C, undefined, B, F)).

-define(RES_UNSUB_FAILED(Id, C, B, F),
    west_msg_utils:build_msg(
        Id, "west", "west:unsubscription_failed", C, undefined, B, F)).

-define(RES_PUB_OK(Id, C, B, F),
    west_msg_utils:build_msg(
        Id, "west", "west:publication_succeeded", C, undefined, B, F)).

-define(RES_PUB_FAILED(Id, C, B, F),
    west_msg_utils:build_msg(
        Id, "west", "west:publication_failed", C, undefined, B, F)).
