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
%%% @doc `WEST'. This module handles subscription's lifecycle. It
%%%      receives events published by other processes from other nodes
%%%      and when events arrives, a callback func is applied.
%%% @end
%%% Created : 03. Oct 2013 9:57 AM
%%%-------------------------------------------------------------------
-module(west_event_handler).

-behaviour(gen_server).

%% API
-export([start_link/3, create/3,
         reg/2, unreg/2, send/4,
         subscribe/2, unsubscribe/2, publish/4,
         set_callback/2,
         get_events/1,
         delete/1]).

%% Callback
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% Includes
-include("west.hrl").
-include("west_int.hrl").

-define(SERVER, ?MODULE).

-record(state, {scope, events = [], cb = ?CALLBACK{}}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start a gen_server to register the subscription and handle
%%      events.
-spec start_link(scope(), cb_spec(), proplist()) -> {ok, pid()} | {error, term()}.
start_link(Scope, CallbackSpec, Opts) ->
  gen_server:start_link(?MODULE, [Scope, CallbackSpec, Opts], []).

%% @doc Create a message-driven handler to handle incoming messages.
%%      This new process is added to the supervisor's tree.
-spec create(scope(), cb_spec(), proplist()) -> supervisor:startchild_ret().
create(Scope, CallbackSpec, Opts) ->
  west_event_handler_sup:start_child(Scope, CallbackSpec, Opts).

%% @doc Register to a point-to-point channel with name `Key'.
%%      All incoming events to the channel `Key' will be handle them
%%      by this process.
-spec reg(server_ref(), key()) -> {ok, key()} | {error, term()}.
reg(ServerRef, Key) ->
  gen_server:call(ServerRef, {reg, Key}).

%% @doc Unregister from a point-to-point channel with name `Key'. This
%%      process won't handle incoming events to the channel any more.
-spec unreg(server_ref(), key()) -> {ok, key()} | {error, term()}.
unreg(ServerRef, Key) ->
  gen_server:call(ServerRef, {unreg, Key}).

%% @doc Send the message `Msg' to point-to-point channel `Key'.
%%      Just one consumer will receive this message.
-spec send(server_ref(), any(), key(), msg()) -> {ok, key()} | {error, term()}.
send(ServerRef, Id, Key, Msg) ->
  gen_server:call(ServerRef, {send, Id, Key, Msg}).

%% @doc Subscribe to a pub/sub channel `Event'. All incoming events
%%      to the channel `Event' will be handle them by this process.
-spec subscribe(server_ref(), event()) -> {ok, event()} | {error, term()}.
subscribe(ServerRef, Event) ->
  gen_server:call(ServerRef, {subscribe, Event}).

%% @doc Delete a subscription from a pub/sub channel `Event'. This
%%      process won't handle incoming events to channel `Event'
%%      any more.
-spec unsubscribe(server_ref(), event()) -> {ok, event()} | {error, term()}.
unsubscribe(ServerRef, Event) ->
  gen_server:call(ServerRef, {unsubscribe, Event}).

%% @doc Publish the message `Msg' to all subscribers to a pub/sub
%%      channel `Event'.
-spec publish(server_ref(), any(), event(), msg()) -> ok.
publish(ServerRef, ETag, Event, Msg) ->
  gen_server:cast(ServerRef, {publish, ETag, Event, Msg}).

%% @doc Sets the callback spec and returns the previous one.
-spec set_callback(server_ref(), cb_spec()) -> {ok, cb_spec()} | {error, term()}.
set_callback(ServerRef, CallbackSpec) ->
  gen_server:call(ServerRef, {set_callback, CallbackSpec}).

%% @doc Get the events which this server is subscribed.
-spec get_events(server_ref()) -> {ok, list()} | {error, term()}.
get_events(ServerRef) ->
  gen_server:call(ServerRef, get_events).

%% @doc Stops this gen_server.
-spec delete(server_ref()) -> ok.
delete(ServerRef) ->
  gen_server:cast(ServerRef, delete).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([Scope, CallbackSpec, Opts]) ->
  Monitors = west_util:keyfind(monitors, Opts, []),
  lists:foreach(fun(I) -> erlang:monitor(process, I) end, Monitors),
  {M, F, A} = CallbackSpec,
  {ok, #state{scope = Scope, cb = ?CALLBACK{mod = M, func = F, args = A}}}.

%% @private
%% @doc Handles the register command.
handle_call({reg, Key}, _From, #state{scope = Scope} = S) ->
  case ?REG(Scope, Key) of
    true -> {reply, {ok, Key}, S};
    {error, Reason} -> {reply, {error, Reason}, S};
    _ -> {reply, {error, unexpected}, S}
  end;

%% @private
%% @doc Handles the unregister command.
handle_call({unreg, Key}, _From, #state{scope = Scope} = S) ->
  case ?UNREG(Scope, Key) of
    true -> {reply, {ok, Key}, S};
    {error, Reason} -> {reply, {error, Reason}, S};
    _ -> {reply, {error, unexpected}, S}
  end;

%% @private
%% @doc Handles the send command.
handle_call({send, Id, Key, Msg}, _From, #state{scope = Scope} = S) ->
  case ?SEND(Scope, Id, Key, Msg) of
    true -> {reply, {ok, Key}, S};
    {error, Reason} -> {reply, {error, Reason}, S};
    _ -> {reply, {error, unexpected}, S}
  end;

%% @private
%% @doc Handles the subscribe command.
handle_call({subscribe, Event},
            _From,
            #state{scope = Scope, events = EvL, cb = Cb} = S) ->
  case ?PS_SUB(Scope, Event) of
    true ->
      NewS = #state{scope = Scope, events = [Event | EvL], cb = Cb},
      {reply, {ok, Event}, NewS};
    {error, Reason} ->
      {reply, {error, Reason}, S};
    _ ->
      {reply, {error, unexpected}, S}
  end;

%% @private
%% @doc Handles the unsubscribe command.
handle_call({unsubscribe, Event},
            _From,
            #state{scope = Scope, events = EvL, cb = Cb} = S) ->
  case ?PS_UNSUB(Scope, Event) of
    true ->
      NewS = #state{scope = Scope, events = lists:delete(Event, EvL), cb = Cb},
      {reply, {ok, Event}, NewS};
    {error, Reason} ->
      {reply, {error, Reason}, S};
    _ ->
      {reply, {error, unexpected}, S}
  end;

%% @private
%% @doc Handles the set_callback command.
handle_call({set_callback, {M, F, A}},
            _From,
            #state{scope = Scope, events = EvL, cb = Cb}) ->
  CbSpec =  ?CALLBACK{mod = M, func = F, args = A},
  {reply, {ok, Cb}, #state{scope = Scope, events = EvL, cb = CbSpec}};

%% @private
%% @doc Handles the get_events command.
handle_call(get_events, _From, #state{events = EvL} = S) ->
  {reply, {ok, EvL}, S};

%% @private
%% @doc Unhandled.
handle_call(_, _, S) ->
  {reply, badarg, S}.

%% @private
%% @doc Handles the publish command.
handle_cast({publish, ETag, Event, Msg}, #state{scope = Scope} = S) ->
  ?PS_PUB(Scope, ETag, Event, Msg),
  {noreply, S};

%% @private
%% @doc Handles the delete command.
handle_cast(delete, S) ->
  {stop, normal, S};

%% @private
%% @doc Unhandled.
handle_cast(_Msg, S) ->
  {noreply, S}.

%% @private
%% @doc Handle incoming events.
%% This callback handle the incoming events from the registered event.
%% When a message arrives to this `EvKey' the callback functions which
%% this server was created `#state{cb=#callback{}}' is applied.
handle_info({ETag, EvKey, Msg},
            #state{cb = ?CALLBACK{mod = M, func = F, args = A}} = S) ->
  case M of
    none when is_function(F) ->
      apply(F, [{ETag, EvKey, Msg}, A]);
    _ ->
      apply(M, F, [{ETag, EvKey, Msg}, A])
  end,
  {noreply, S};

%% @private
%% @doc Monitor has benn triggered, stop this gen_server.
handle_info({'DOWN', _Ref, process, Pid, _Reason}, S) ->
  error_logger:info_msg(
    "gen_server ~p stopped, monitor of ~p triggered.", [self(), Pid]),
  {stop, normal, S};

%% @private
%% @doc Unhandled.
handle_info(_, S) ->
  {noreply, S}.

%% @private
%% @doc Finish the gen_server.
terminate(_Reason, _State) ->
  ok.

%% @private
%% @doc Unhandled.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
