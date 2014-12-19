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
%%% Created : 05. Nov 2013 12:12 PM
%%%-------------------------------------------------------------------
-module(west_lib).

%% API
-export([reg/4,
         unreg/2,
         send/4,
         sub/4,
         unsub/2,
         pub/4]).

-include("west_int.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc reg/4.
%% Register to a point-to-point channel with name `Key'. All incoming
%% events to the channel `Key' will be handle them by a GS created
%% by this process.
%% <br/>
%% Creates a GS event handler and register it into Gproc, locally.
%% This GS will handle the incoming messages.
%% <br/>
%% <li>Scope: Gproc scope.</li>
%% <li>Ref: Unique reference to the GS that will be created.</li>
%% <li>Key: Key which the GS will be registered.</li>
%% <li>CbSpec: Callback specification. This will be called when messages
%% arrives.</li>
%%
%% @spec reg(Scope, Ref, Key, CbSpec) -> Reply :: term()
%% Scope = atom()
%% Ref = any()
%% Key = atom()
%% CbSpec = {Mod :: atom(), Fun :: atom(), Args :: list()}
reg(Scope, Ref, Key, CbSpec) ->
    Name = west_utils:build_name([Ref, Key]),
    case whereis(Name) of
        undefined ->
            {ok, Pid} = west_event_handler:create(Scope,
                                                  CbSpec,
                                                  [{monitors, [Ref]}]),
            register(Name, Pid),
            case west_event_handler:reg(Name, Key) of
                {ok, _} ->
                    {ok, registration_succeeded, Key};
                {error, _} ->
                    west_event_handler:delete(Name),
                    {error, registration_denied, Key}
            end;
        _ ->
            {error, registration_already_exist, Key}
    end.

%% @doc unreg/2.
%% Unregister from a point-to-point channel with name `Key'. The
%% created GS process won't handle incoming events to channel `Key'
%% any more.
%% <br/>
%% Destroy the GS event handler in order to delete the registration
%% from Gproc.
%% <br/>
%% <li>Ref: Unique reference to the GS that will be created.</li>
%% <li>Key: Key which the GS was registered.</li>
%%
%% @spec unreg(Ref :: any(), Key :: atom()) -> Reply :: term()
unreg(Ref, Key) ->
    Name = west_utils:build_name([Ref, Key]),
    case whereis(Name) of
        undefined ->
            {error, registration_not_found, Key};
        Pid ->
            case ?PROC_TYPE(Pid) of
                n ->
                    west_event_handler:delete(Name),
                    {ok, unregistration_succeeded, Key};
                _ ->
                    {error, registration_not_found, Key}
            end
    end.

%% @doc send/4.
%% Send the message `Msg' to point-to-point channel `Key'. Just one
%% consumer will receive this message.
%% <br/>
%% Sends the given message `Msg' to a `Key'. If the registration to
%% `Key' exist, message will be received by the registered GS. If
%% registration doesn't exist, send will fail.
%% <br/>
%% <li>Scope: Gproc scope.</li>
%% <li>ETag: ID of the sender.</li>
%% <li>Key: Key which the GS was registered.</li>
%% <li>Msg: Message that will send.</li>
%%
%% @spec send(Scope, ETag, Key, Msg) -> Reply :: term()
%% Scope = atom()
%% ETag = string()
%% Key = atom()
%% Msg = binary() | list()
send(Scope, ETag, Key, Msg) ->
    F = fun() ->
            case ?SEND(Scope, ETag, Key, Msg) of
                true ->
                    {ok, sending_succeeded, Key};
                _ ->
                    {error, sending_failed, Key}
            end
        end,
    case Scope of
        g ->
            case ?WHERE(Scope, Key) of
                undefined ->
                    {error, sending_failed, Key};
                {error, _} ->
                    {error, sending_failed, Key};
                _ ->
                    F()
            end;
        _ ->
            F()
    end.

%% @doc sub/4.
%% Subscribe to a pub/sub channel `Event'. All incoming events to the
%% channel `Event' will be handle them by GS created by this process.
%% <br/>
%% Creates a GS event handler and subscribe it into Gproc, in order
%% to handle the subscription lifecycle and handle the published
%% messages to `Event'.
%% <br/>
%% <li>Scope: Gproc scope.</li>
%% <li>Ref: Unique reference to the GS that will be created.</li>
%% <li>Event: Event which the GS will be subscribed.</li>
%% <li>CbSpec: Callback specification. This will be called when messages
%% arrives.</li>
%%
%% @spec sub(Scope, Ref, Event, CbSpec) -> Reply :: term()
%% Scope = atom()
%% Ref = any()
%% Event = atom()
%% CbSpec = {Mod :: atom(), Fun :: atom(), Args :: list()}
sub(Scope, Ref, Event, CbSpec) ->
    Name = west_utils:build_name([Ref, Event]),
    case whereis(Name) of
        undefined ->
            {ok, Pid} = west_event_handler:create(Scope,
                                                  CbSpec,
                                                  [{monitors, [Ref]}]),
            register(Name, Pid),
            case west_event_handler:subscribe(Name, Event) of
                {ok, _} ->
                    {ok, subscription_succeeded, Event};
                {error, _} ->
                    west_event_handler:delete(Name),
                    {error, subscription_failed, Event}
            end;
        _ ->
            {error, subscription_already_exist, Event}
    end.

%% @doc unsub/2.
%% Delete a subscription from a pub/sub channel `Event'.The
%% created GS process won't handle incoming events to channel `Event'
%% any more.
%% <br/>
%% Destroy the GS event handler in order to delete the subscription
%% from Gproc. Ends the subscription lifecycle.
%% <br/>
%% <li>Ref: Unique reference to the GS that will be created.</li>
%% <li>Event: Event which the GS was subscribed.</li>
%%
%% @spec unsub(Ref :: any(), Event :: atom()) -> Reply :: term()
unsub(Ref, Event) ->
    Name = west_utils:build_name([Ref, Event]),
    case whereis(Name) of
        undefined ->
            {error, subscription_not_found, Event};
        Pid ->
            case ?PROC_TYPE(Pid) of
                p ->
                    west_event_handler:delete(Name),
                    {ok, unsubscription_succeeded, Event};
                _ ->
                    {error, subscription_not_found, Event}
            end
    end.

%% @doc pub/4.
%% Publish the message `Msg' to all subscribers to a pub/sub channel
%% `Event'.
%% <br/>
%% Publishes the given message `Msg' into the a `Event'. If the
%% subscription to `Event' exist, message will be received by the
%% subscribed GS. If subscription doesn't exist, publish will fail.
%% <br/>
%% <li>Scope: Gproc scope.</li>
%% <li>ETag: ID of the sender.</li>
%% <li>Event: Event which the GS was registered.</li>
%% <li>Msg: Message that will send.</li>
%%
%% @spec pub(Scope, ETag, Event, Msg) -> Reply :: term()
%% Scope = atom()
%% ETag = string()
%% Event = atom()
%% Msg = binary() | list()
pub(Scope, ETag, Event, Msg) ->
    case ?PS_PUB(Scope, ETag, Event, Msg) of
        true ->
            {ok, publication_succeeded, Event};
        _ ->
            {error, publication_failed, Event}
    end.
