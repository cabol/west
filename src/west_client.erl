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
%%% @doc This gen_server is a native Erlang WEST client or wrapper.
%%% @end
%%% Created : 10. Nov 2013 10:53 AM
%%%-------------------------------------------------------------------
-module(west_client).

-behaviour(gen_server).

%% API
-export([start_link/3,
         reg/2,
         unreg/2,
         send/3,
         sub/2,
         unsub/2,
         pub/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("west.hrl").

-define(SERVER, ?MODULE).

-record(state, {server=?WEST_SERVER{}, opts}).

-type channel() :: string().

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the gen_server.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(iolist(), cb_spec(), proplist()) ->
                 {ok, pid()} | ignore | {error, term()}).
start_link(Key, CallbackSpec, Opts) ->
    gen_server:start_link(?MODULE, [Key, CallbackSpec, Opts], []).

%%--------------------------------------------------------------------
%% @doc
%% Register to a point-to-point channel with name `Channel'. All
%% incoming events to the channel `Key' will be handle them by the
%% callback spec.
%%
%% @end
%%--------------------------------------------------------------------
-spec reg(server_ref(), channel()) -> {ok | error, msg_spec()}.
reg(ServerRef, Channel) ->
    gen_server:call(ServerRef, {reg, Channel}).

%%--------------------------------------------------------------------
%% @doc
%% Unregister from a point-to-point channel with name `Channel'.
%%
%% @end
%%--------------------------------------------------------------------
-spec unreg(server_ref(), channel()) -> {ok | error, msg_spec()}.
unreg(ServerRef, Channel) ->
    gen_server:call(ServerRef, {unreg, Channel}).

%%--------------------------------------------------------------------
%% @doc
%% Send the message `Msg' to point-to-point channel `Channel'. Just
%% one consumer will receive this message.
%%
%% @end
%%--------------------------------------------------------------------
-spec send(server_ref(), channel(), msg()) -> {ok | error, msg_spec()}.
send(ServerRef, Channel, Msg) ->
    gen_server:call(ServerRef, {send, Channel, Msg}).

%%--------------------------------------------------------------------
%% @doc
%% Subscribe to a pub/sub channel `Channel'. All incoming events to
%% the channel `Channel' will be handle them by the callback spec.
%%
%% @end
%%--------------------------------------------------------------------
-spec sub(server_ref(), channel()) -> {ok | error, msg_spec()}.
sub(ServerRef, Channel) ->
    gen_server:call(ServerRef, {sub, Channel}).

%%--------------------------------------------------------------------
%% @doc
%% Deletes a subscription from a pub/sub channel `Channel'.
%%
%% @end
%%--------------------------------------------------------------------
-spec unsub(server_ref(), channel()) -> {ok | error, msg_spec()}.
unsub(ServerRef, Channel) ->
    gen_server:call(ServerRef, {unsub, Channel}).

%%--------------------------------------------------------------------
%% @doc
%% Publish the message `Msg' to all subscribers to a pub/sub channel
%% `Channel'.
%%
%% @end
%%--------------------------------------------------------------------
-spec pub(server_ref(), channel(), msg()) -> {ok | error, msg_spec()}.
pub(ServerRef, Channel, Msg) ->
    gen_server:call(ServerRef, {pub, Channel, Msg}).

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
init([Key, CallbackSpec, Opts]) ->
    Dist = case application:get_env(west, dist) of
               {ok, Env0} -> Env0;
               _          -> west_dist
           end,
    Scope = case Dist of
                gproc_dist -> g;
                _          -> l
            end,
    WDist = case application:get_env(west, west_dist) of
                {ok, Env1} -> Env1;
                _          -> [{n, 1}, {q, 1}]
            end,
    Name = west_utils:build_name([Key, self(), erlang:now()]),
    register(Name, self()),
    Server = ?WEST_SERVER{name=Name,
                          key=Key,
                          dist=Dist,
                          west_dist=WDist,
                          scope=Scope,
                          cb=CallbackSpec},
    {ok, #state{server=Server, opts=Opts}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle register command.
%%
%% @end
%%--------------------------------------------------------------------
handle_call({reg, Channel}, _From, #state{server=WS}=S) ->
    MsgSpec = ?MSG{id=undefined, channel=Channel},
    Reply = west_protocol_handler:handle_event(register, MsgSpec, WS),
    {reply, Reply, S};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle unregister command.
%%
%% @end
%%--------------------------------------------------------------------
handle_call({unreg, Channel}, _From, #state{server=WS}=S) ->
    MsgSpec = ?MSG{id=undefined, channel=Channel},
    Reply = west_protocol_handler:handle_event(unregister, MsgSpec, WS),
    {reply, Reply, S};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle send command.
%%
%% @end
%%--------------------------------------------------------------------
handle_call({send, Channel, Msg}, _From, #state{server=WS}=S) ->
    MsgSpec = ?MSG{id=undefined, channel=Channel, data=Msg},
    Reply = west_protocol_handler:handle_event(send, MsgSpec, WS),
    {reply, Reply, S};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle subscribe command.
%%
%% @end
%%--------------------------------------------------------------------
handle_call({sub, Channel}, _From, #state{server=WS}=S) ->
    MsgSpec = ?MSG{id=undefined, channel=Channel},
    Reply = west_protocol_handler:handle_event(subscribe, MsgSpec, WS),
    {reply, Reply, S};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle unsubscribe command.
%%
%% @end
%%--------------------------------------------------------------------
handle_call({unsub, Channel}, _From, #state{server=WS}=S) ->
    MsgSpec = ?MSG{id=undefined, channel=Channel},
    Reply = west_protocol_handler:handle_event(unsubscribe, MsgSpec, WS),
    {reply, Reply, S};

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle publish command.
%%
%% @end
%%--------------------------------------------------------------------
handle_call({pub, Channel, Msg}, _From, #state{server=WS}=S) ->
    MsgSpec = ?MSG{id=undefined, channel=Channel, data=Msg},
    Reply = west_protocol_handler:handle_event(publish, MsgSpec, WS),
    {reply, Reply, S}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%%
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%%
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
