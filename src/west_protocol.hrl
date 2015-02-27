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
%%% @doc Protocol Macros.
%%% @end
%%% Created : 06. Oct 2013 8:45 AM
%%%-------------------------------------------------------------------

-define(RES_CONN_ESTABLISHED(F),
  west_msg:build_msg(
    undefined, west, connection_established, undefined, undefined, F)
).

-define(RES_PONG(Id, F),
  west_msg:build_msg(
    Id, west, pong, undefined, undefined, F)
).

-define(RES_INTERNAL_ERROR(Id, F),
  west_msg:build_msg(
    Id, west, internal_error, undefined, undefined, F)
).

-define(RES_BAD_REQUEST(F),
  west_msg:build_msg(
    undefined, west, bad_request, undefined, undefined, F)
).

-define(RES_ACTION_NOT_ALLOWED(Id, C, F),
  west_msg:build_msg(
    Id, west, action_not_allowed, C, undefined, F)
).

-define(RES_CH_NEW_MSG(Id, ET, C, B, F),
  west_msg:build_msg(
    Id, ET, new_message, C, B, F)
).

-define(RES_CH_NOT_FOUND(Id, C, F),
  west_msg:build_msg(
    Id, west, channel_not_found, C, undefined, F)
).

-define(RES_CH_CREATION_OK(Id, C, F),
  west_msg:build_msg(
    Id, west, channel_creation_succeeded, C, undefined, F)
).

-define(RES_CH_ALREADY_EXIST(Id, C, F),
  west_msg:build_msg(
    Id, west, channel_already_exist, C, undefined, F)
).

-define(RES_CH_CREATION_FAILED(Id, C, F),
  west_msg:build_msg(
    Id, west, channel_creation_failed, C, undefined, F)
).

-define(RES_CH_DELETE_OK(Id, C, F),
  west_msg:build_msg(
    Id, west, channel_delete_succeeded, C, undefined, F)
).

-define(RES_CH_DELETE_FAILED(Id, C, F),
  west_msg:build_msg(
    Id, west, channel_delete_failed, C, undefined, F)
).

-define(RES_REG_OK(Id, C, F),
  west_msg:build_msg(
    Id, west, registration_succeeded, C, undefined, F)
).

-define(RES_REG_FAILED(Id, C, F),
  west_msg:build_msg(
    Id, west, registration_failed, C, undefined, F)
).

-define(RES_REG_ALREADY_EXIST(Id, C, F),
  west_msg:build_msg(
    Id, west, registration_already_exist, C, undefined, F)
).

-define(RES_REG_NOT_FOUND(Id, C, F),
  west_msg:build_msg(
    Id, west, registration_not_found, C, undefined, F)
).

-define(RES_REG_DENIED(Id, C, F),
  west_msg:build_msg(
    Id, west, registration_denied, C, undefined, F)
).

-define(RES_UNREG_OK(Id, C, F),
  west_msg:build_msg(
    Id, west, unregistration_succeeded, C, undefined, F)
).

-define(RES_UNREG_FAILED(Id, C, F),
  west_msg:build_msg(
    Id, west, unregistration_failed, C, undefined, F)
).

-define(RES_SEND_OK(Id, C, F),
  west_msg:build_msg(
    Id, west, sending_succeeded, C, undefined, F)
).

-define(RES_SEND_FAILED(Id, C, F),
  west_msg:build_msg(
    Id, west, sending_failed, C, undefined, F)
).

-define(RES_SUB_OK(Id, C, F),
  west_msg:build_msg(
    Id, west, subscription_succeeded, C, undefined, F)
).

-define(RES_SUB_ALREADY_EXIST(Id, C, F),
  west_msg:build_msg(
    Id, west, subscription_already_exist, C, undefined, F)
).

-define(RES_SUB_NOT_FOUND(Id, C, F),
  west_msg:build_msg(
    Id, west, subscription_not_found, C, undefined, F)
).

-define(RES_SUB_FAILED(Id, C, F),
  west_msg:build_msg(
    Id, west, subscription_failed, C, undefined, F)
).

-define(RES_UNSUB_OK(Id, C, F),
  west_msg:build_msg(
    Id, west, unsubscription_succeeded, C, undefined, F)
).

-define(RES_UNSUB_FAILED(Id, C, F),
  west_msg:build_msg(
    Id, west, unsubscription_failed, C, undefined, F)
).

-define(RES_PUB_OK(Id, C, F),
  west_msg:build_msg(
    Id, west, publication_succeeded, C, undefined, F)
).

-define(RES_PUB_FAILED(Id, C, F),
  west_msg:build_msg(
    Id, west, publication_failed, C, undefined, F)
).
