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
%%% @doc `WEST'. This module handles subscription's lifecycle. It
%%%      receives events published by other processes from other nodes
%%%      and when events arrives, a callback func is applied.
%%% @end
%%% Created : 03. Oct 2013 9:57 AM
%%%-------------------------------------------------------------------
-module(west_event_handler).

-behaviour(gen_server).

%% API
-export([start_link/3,
         create/3,
         reg/2, reg/3,
         unreg/2, unreg/3,
         send/4, send/5,
         subscribe/2, subscribe/3,
         unsubscribe/2, unsubscribe/3,
         publish/4, publish/5,
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

-include("west.hrl").
-include("west_int.hrl").

-type dist_spec() :: {bucket(), key(), proplist()}.

-define(SERVER, ?MODULE).

-record(state, {scope, events=[], cb=?CALLBACK_SPEC{}}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Start a gen_server to register the subscription and handle events.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(scope(), cb_spec(), proplist()) ->
                 {ok, pid()} | {error, term()}.
start_link(Scope, CallbackSpec, Opts) ->
    gen_server:start_link(?MODULE, [Scope, CallbackSpec, Opts], []).

%%--------------------------------------------------------------------
%% @doc
%% Create a message-driven handler to handle incoming messages. This
%% new process is added to the supervisor's tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec create(scope(), cb_spec(), proplist()) ->
             {ok, pid()} | {error, term()}.
create(Scope, CallbackSpec, Opts) ->
    west_event_handler_sup:start_child(Scope, CallbackSpec, Opts).

%%--------------------------------------------------------------------
%% @doc
%% Register to a point-to-point channel with name `Key'. All incoming
%% events to the channel `Key' will be handle them by this process.
%%
%% @end
%%--------------------------------------------------------------------
-spec reg(server_ref(), key()) -> {ok, key()} | {error, term()}.
reg(ServerRef, Key) ->
    gen_server:call(ServerRef, {reg, Key}).

%%--------------------------------------------------------------------
%% @doc
%% Same as previous, but the register is executed distributed.
%%
%% @end
%%--------------------------------------------------------------------
-spec reg(dist_spec(), server_ref(), key()) ->
          {ok, key()} | {error, term()}.
reg(DistSpec, ServerRef, Key) ->
    gen_server:call(ServerRef, {reg_dist, DistSpec, ServerRef, Key}).

%%--------------------------------------------------------------------
%% @doc
%% Unregister from a point-to-point channel with name `Key'. This
%% process won't handle incoming events to channel `Key' any more.
%%
%% @end
%%--------------------------------------------------------------------
-spec unreg(server_ref(), key()) -> {ok, key()} | {error, term()}.
unreg(ServerRef, Key) ->
    gen_server:call(ServerRef, {unreg, Key}).

%%--------------------------------------------------------------------
%% @doc
%% Same as previous, but the unregister is executed distributed.
%%
%% @end
%%--------------------------------------------------------------------
-spec unreg(dist_spec(), server_ref(), key()) ->
            {ok, key()} | {error, term()}.
unreg(DistSpec, ServerRef, Key) ->
    gen_server:call(ServerRef, {unreg_dist, DistSpec, ServerRef, Key}).

%%--------------------------------------------------------------------
%% @doc
%% Send the message `Msg' to point-to-point channel `Key'. Just one
%% consumer will receive this message.
%%
%% @end
%%--------------------------------------------------------------------
-spec send(server_ref(), any(), key(), msg()) ->
           {ok, key()} | {error, term()}.
send(ServerRef, Id, Key, Msg) ->
    gen_server:call(ServerRef, {send, Id, Key, Msg}).

%%--------------------------------------------------------------------
%% @doc
%% Same as previous, but the send is executed distributed.
%%
%% @end
%%--------------------------------------------------------------------
-spec send(dist_spec(), server_ref(), any(), key(), msg()) ->
           {ok, key()} | {error, term()}.
send(DistSpec, ServerRef, Id, Key, Msg) ->
    gen_server:call(ServerRef, {send_dist, DistSpec, Id, Key, Msg}).

%%--------------------------------------------------------------------
%% @doc
%% Subscribe to a pub/sub channel `Event'. All incoming events to the
%% channel `Event' will be handle them by this process.
%%
%% @end
%%--------------------------------------------------------------------
-spec subscribe(server_ref(), event()) -> {ok, event()} | {error, term()}.
subscribe(ServerRef, Event) ->
    gen_server:call(ServerRef, {subscribe, Event}).

%%--------------------------------------------------------------------
%% @doc
%% Same as previous, but the subscription is executed distributed.
%%
%% @end
%%--------------------------------------------------------------------
-spec subscribe(dist_spec(), server_ref(), key()) ->
                {ok, key()} | {error, term()}.
subscribe(DistSpec, ServerRef, Event) ->
    gen_server:call(ServerRef, {subscribe_dist, DistSpec, ServerRef, Event}).

%%--------------------------------------------------------------------
%% @doc
%% Delete a subscription from a pub/sub channel `Event'. This process
%% won't handle incoming events to channel `Event' any more.
%%
%% @end
%%--------------------------------------------------------------------
-spec unsubscribe(server_ref(), event()) -> {ok, event()} | {error, term()}.
unsubscribe(ServerRef, Event) ->
    gen_server:call(ServerRef, {unsubscribe, Event}).

%%--------------------------------------------------------------------
%% @doc
%% Same as previous, but the unsubscription is executed distributed.
%%
%% @end
%%--------------------------------------------------------------------
-spec unsubscribe(dist_spec(), server_ref(), key()) ->
                  {ok, key()} | {error, term()}.
unsubscribe(DistSpec, ServerRef, Event) ->
    gen_server:call(ServerRef, {unsubscribe_dist, DistSpec, ServerRef, Event}).

%%--------------------------------------------------------------------
%% @doc
%% Publish the message `Msg' to all subscribers to a pub/sub channel
%% `Event'.
%%
%% @end
%%--------------------------------------------------------------------
-spec publish(server_ref(), any(), event(), msg()) -> ok.
publish(ServerRef, ETag, Event, Msg) ->
    gen_server:cast(ServerRef, {publish, ETag, Event, Msg}).

%%--------------------------------------------------------------------
%% @doc
%% Same as previous, but the publish is executed distributed.
%%
%% @end
%%--------------------------------------------------------------------
-spec publish(dist_spec(), server_ref(), any(), key(), msg()) -> ok.
publish(DistSpec, ServerRef, ETag, Event, Msg) ->
    gen_server:cast(ServerRef, {publish_dist, DistSpec, ETag, Event, Msg}).

%%--------------------------------------------------------------------
%% @doc
%% Sets the callback spec and returns the previous one.
%%
%% @end
%%--------------------------------------------------------------------
-spec set_callback(server_ref(), cb_spec()) ->
                   {ok, cb_spec()} | {error, term()}.
set_callback(ServerRef, CallbackSpec) ->
    gen_server:call(ServerRef, {set_callback, CallbackSpec}).

%%--------------------------------------------------------------------
%% @doc
%% Get the events which this server is subscribed.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_events(server_ref())-> {ok, list()} | {error, term()}.
get_events(ServerRef) ->
    gen_server:call(ServerRef, get_events).

%%--------------------------------------------------------------------
%% @doc
%% Stops this gen_server.
%%
%% @end
%%--------------------------------------------------------------------
-spec delete(server_ref()) -> ok.
delete(ServerRef) ->
    gen_server:cast(ServerRef, delete).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Scope, CallbackSpec, Opts]) ->
    Monitors = proplists:get_value(monitors, Opts, []),
    lists:foreach(fun(I) -> erlang:monitor(process, I) end, Monitors),
    {M, F, A} = CallbackSpec,
    {ok, #state{scope=Scope, cb=?CALLBACK_SPEC{mod=M, func=F, args=A}}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles the register command.
%%
%% @end
%%--------------------------------------------------------------------
handle_call({reg, Key}, _From, #state{scope=Scope}=S) ->
    case ?REG(Scope, Key) of
        true            -> {reply, {ok, Key}, S};
        {error, Reason} -> {reply, {error, Reason}, S};
        _               -> {reply, {error, "Unexpected error."}, S}
    end;


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles the distributed register command.
%%
%% @end
%%--------------------------------------------------------------------
handle_call({reg_dist, {B, K, Opts}, Name, Key},
            _From,
            #state{scope=Scope}=S) ->
    F = fun(Arg0, Arg1, Arg2) ->
            ?REG_EXT(Arg0, Arg1, Arg2)
        end,
    case west_dist:cmd(B, K, {none, F, [{Name, node()}, Scope, Key]}, Opts) of
        true            -> {reply, {ok, Key}, S};
        {error, Reason} -> {reply, {error, Reason}, S};
        _               -> {reply, {error, "Unexpected error."}, S}
    end;

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles the unregister command.
%%
%% @end
%%--------------------------------------------------------------------
handle_call({unreg, Key}, _From, #state{scope=Scope}=S) ->
    case ?UNREG(Scope, Key) of
        true            -> {reply, {ok, Key}, S};
        {error, Reason} -> {reply, {error, Reason}, S};
        _               -> {reply, {error, "Unexpected error."}, S}
    end;

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles the distributed unregister command.
%%
%% @end
%%--------------------------------------------------------------------
handle_call({unreg_dist, {B, K, Opts}, Name, Key},
            _From,
            #state{scope=Scope}=S) ->
    F = fun(Arg0, Arg1, Arg2) ->
            ?UNREG_EXT(Arg0, Arg1, Arg2)
        end,
    case west_dist:cmd(B, K, {none, F, [{Name, node()}, Scope, Key]}, Opts) of
        true            -> {reply, {ok, Key}, S};
        {error, Reason} -> {reply, {error, Reason}, S};
        _               -> {reply, {error, "Unexpected error."}, S}
    end;

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles the send command.
%%
%% @end
%%--------------------------------------------------------------------
handle_call({send, Id, Key, Msg}, _From, #state{scope=Scope}=S) ->
    case ?SEND(Scope, Id, Key, Msg) of
        true            -> {reply, {ok, Key}, S};
        {error, Reason} -> {reply, {error, Reason}, S};
        _               -> {reply, {error, "Unexpected error."}, S}
    end;

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles the distributed send command.
%%
%% @end
%%--------------------------------------------------------------------
handle_call({send_dist, {B, K, Opts}, Id, Key, Msg},
            _From,
            #state{scope=Scope}=S) ->
    F = fun(Arg0, Arg1, Arg2, Arg3) ->
            ?SEND(Arg0, Arg1, Arg2, Arg3)
        end,
    case west_dist:cmd(B, K, {none, F, [Scope, Id, Key, Msg]}, Opts) of
        true            -> {reply, {ok, Key}, S};
        {error, Reason} -> {reply, {error, Reason}, S};
        _               -> {reply, {error, "Unexpected error."}, S}
    end;

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles the subscribe command.
%%
%% @end
%%--------------------------------------------------------------------
handle_call({subscribe, Event},
            _From,
            #state{scope=Scope, events=EvL, cb=Cb}=S) ->
    case ?PS_SUB(Scope, Event) of
        true ->
            NewS = #state{scope=Scope, events=[Event|EvL], cb=Cb},
            {reply, {ok, Event}, NewS};
        {error, Reason} ->
            {reply, {error, Reason}, S};
        _ ->
            {reply, {error, "Unexpected error."}, S}
    end;

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles the distributed subscribe command.
%%
%% @end
%%--------------------------------------------------------------------
handle_call({subscribe_dist, {B, K, Opts}, Name, Event},
            _From,
            #state{scope=Scope, events=EvL, cb=Cb}=S) ->
    F = fun(Arg0, Arg1, Arg2) ->
            ?PS_SUB_EXT(Arg0, Arg1, Arg2)
        end,
    Args = [{Name, node()}, Scope, Event],
    case west_dist:cmd(B, K, {none, F, Args}, Opts) of
        true ->
            NewS = #state{scope=Scope, events=[Event|EvL], cb=Cb},
            {reply, {ok, Event}, NewS};
        {error, Reason} ->
            {reply, {error, Reason}, S};
        _ ->
            {reply, {error, "Unexpected error."}, S}
    end;

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles the unsubscribe command.
%%
%% @end
%%--------------------------------------------------------------------
handle_call({unsubscribe, Event},
            _From,
            #state{scope=Scope, events=EvL, cb=Cb}=S) ->
    case ?PS_UNSUB(Scope, Event) of
        true ->
            NewS = #state{scope=Scope, events=lists:delete(Event, EvL), cb=Cb},
            {reply, {ok, Event}, NewS};
        {error, Reason} ->
            {reply, {error, Reason}, S};
        _ ->
            {reply, {error, "Unexpected error."}, S}
    end;

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles the distributed unsubscribe command.
%%
%% @end
%%--------------------------------------------------------------------
handle_call({unsubscribe_dist, {B, K, Opts}, Name, Event},
            _From,
            #state{scope=Scope, events=EvL, cb=Cb}=S) ->
    F = fun(Arg0, Arg1, Arg2) ->
            ?PS_UNSUB_EXT(Arg0, Arg1, Arg2)
        end,
    Args = [{Name, node()}, Scope, Event],
    case west_dist:cmd(B, K, {none, F, Args}, Opts) of
        true ->
            NewS = #state{scope=Scope, events=lists:delete(Event, EvL), cb=Cb},
            {reply, {ok, Event}, NewS};
        {error, Reason} ->
            {reply, {error, Reason}, S};
        _ ->
            {reply, {error, "Unexpected error."}, S}
    end;

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles the set_callback command.
%%
%% @end
%%--------------------------------------------------------------------
handle_call({set_callback, {M, F, A}},
            _From,
            #state{scope=Scope, events=EvL, cb=Cb}) ->
    {reply, {ok, Cb}, #state{scope=Scope,
                             events=EvL,
                             cb=?CALLBACK_SPEC{mod=M, func=F, args=A}}};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles the get_events command.
%%
%% @end
%%--------------------------------------------------------------------
handle_call(get_events, _From, #state{events=EvL}=S) ->
    {reply, {ok, EvL}, S};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Unhandled.
%%
%% @end
%%--------------------------------------------------------------------
handle_call(_, _, S) ->
    {reply, badarg, S}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles the publish command.
%%
%% @end
%%--------------------------------------------------------------------
handle_cast({publish, ETag, Event, Msg}, #state{scope=Scope}=S) ->
    ?PS_PUB(Scope, ETag, Event, Msg),
    {noreply, S};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles the distributed publish command.
%%
%% @end
%%--------------------------------------------------------------------
handle_cast({publish_dist, {B, K, Opts}, ETag, Event, Msg},
            #state{scope=Scope}=S) ->
    F = fun(Arg0, Arg1, Arg2, Arg3) ->
            ?PS_PUB(Arg0, Arg1, Arg2, Arg3)
        end,
    west_dist:cmd(B, K, {none, F, [Scope, ETag, Event, Msg]}, Opts),
    {noreply, S};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles the delete command.
%%
%% @end
%%--------------------------------------------------------------------
handle_cast(delete, S) ->
    {stop, normal, S};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Unhandled.
%%
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, S) ->
    {noreply, S}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This callback handle the incoming events from the registered event.
%% When a message arrives to this `EvKey' the callback functions which
%% this server was created `#state{cb=#callback{}}' is applied.
%%
%% @end
%%--------------------------------------------------------------------
handle_info({ETag, EvKey, Msg},
            #state{cb=?CALLBACK_SPEC{mod=M, func=F, args=A}}=S) ->
    case M of
        none when is_function(F) ->
            apply(F, [{ETag, EvKey, Msg}, A]);
        _ ->
            apply(M, F, [{ETag, EvKey, Msg}, A])
    end,
    {noreply, S};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Monitor has benn triggered, stop this gen_server.
%%
%% @end
%%--------------------------------------------------------------------
handle_info({'DOWN', _Ref, process, Pid, _Reason}, S) ->
    error_logger:info_msg("gen_server ~p stopped, monitor of ~p triggered.",
                          [self(), Pid]),
    {stop, normal, S};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Unhandled.
%%
%% @end
%%--------------------------------------------------------------------
handle_info(_, S) ->
    {noreply, S}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Finish the gen_server.
%%
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Unhandled.
%%
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
