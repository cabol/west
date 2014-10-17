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
%%% @copyright (C) 2014, <Carlos Andres Bolaños>, All Rights Reserved.
%%% @doc WEST tests.
%%% @end
%%% Created : 05. Jul 2014 7:00 AM
%%%-------------------------------------------------------------------
-module(west_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

west_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun(SetupData) ->
         {inorder,
          [{spawn, ?_test(?debugVal(t_west_reg(SetupData)))},
           {spawn, ?_test(?debugVal(t_west_send(SetupData)))},
           {spawn, ?_test(?debugVal(t_west_unreg(SetupData)))},
           {spawn, ?_test(?debugVal(t_west_sub(SetupData)))},
           {spawn, ?_test(?debugVal(t_west_pub(SetupData)))},
           {spawn, ?_test(?debugVal(t_west_unsub(SetupData)))},
           {spawn, ?_test(?debugVal(t_west_pub2(SetupData)))}]
         }
     end}.

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    ets:new(west_test, [duplicate_bag, public, named_table]),
    F = fun(Event, Args) ->
            io:format("Event: ~p~nArgs: ~p~n", [Event, Args]),
            {Usr, _, Msg} = Event,
            ets:insert(west_test, {west_utils:build_name([Usr, Msg]), Msg})
        end,
    SpawnWest = fun(Usr) ->
                    {ok, Pid} = west:start_link(Usr, {none, F, ["Hi!"]}, []),
                    Pid
                end,
    Pids = [{K, SpawnWest("u" ++ integer_to_list(K))} || K <- lists:seq(1, 3)],
    Apps = west_utils:start_app_deps(west),
    {Pids, Apps}.

stop({Pids, Apps}) ->
    [west:stop(Pid) || {_, Pid} <- Pids],
    [application:stop(App) || {_, App} <- Apps].

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

t_west_reg({Pids, _}) ->
    %% register Pid1
    Pid1 = west_utils:keyfind(1, Pids),
    {ok, {message, Res0, Ch0, _, _, _}} = west:reg(Pid1, "r1"),
    ?assertEqual("registration_succeeded", Res0),
    ?assertEqual("r1", Ch0),
    {ok, {message, Res1, Ch1, _, _, _}} = west:reg(Pid1, "r1"),
    ?assertEqual("registration_already_exist", Res1),
    ?assertEqual("r1", Ch1),
    %% register Pid2
    Pid2 = west_utils:keyfind(2, Pids),
    {ok, {message, Res2, Ch2, _, _, _}} = west:reg(Pid2, "r2"),
    ?assertEqual("registration_succeeded", Res2),
    ?assertEqual("r2", Ch2),
    {ok, {message, Res2a, Ch2a, _, _, _}} = west:reg(Pid2, "r1"),
    ?assertEqual("registration_denied", Res2a),
    ?assertEqual("r1", Ch2a),
    %% register Pid3
    Pid3 = west_utils:keyfind(3, Pids),
    {ok, {message, Res3, Ch3, _, _, _}} = west:reg(Pid3, "r3"),
    ?assertEqual("registration_succeeded", Res3),
    ?assertEqual("r3", Ch3).

t_west_send({Pids, _}) ->
    %% send msg from Pid1 to 'r1' channel
    Pid1 = west_utils:keyfind(1, Pids),
    {ok, {message, Res0, Ch0, _, _, _}} = west:send(Pid1, "r1", "M11"),
    ?assertEqual("sending_succeeded", Res0),
    ?assertEqual("r1", Ch0),
    timer:sleep(100),
    [{_, M11}] = ets:lookup(west_test, west_utils:build_name(["u1", "M11"])),
    ?assertEqual("M11", M11),
    %% try to send msg from Pid1 to undefined 'rX' channel
    {ok, {message, Res1, Ch1, _, _, _}} = west:send(Pid1, "rX", "M1X"),
    ?assertEqual("registration_not_found", Res1),
    ?assertEqual("rX", Ch1),
    %% send msg from Pid1 to 'r2' channel
    {ok, {message, Res2, Ch2, _, _, _}} = west:send(Pid1, "r2", "M12"),
    ?assertEqual("sending_succeeded", Res2),
    ?assertEqual("r2", Ch2),
    timer:sleep(100),
    [{_, M12}] = ets:lookup(west_test, west_utils:build_name(["u1", "M12"])),
    ?assertEqual("M12", M12),
    %% send msg from Pid2 to 'r3' channel
    Pid2 = west_utils:keyfind(2, Pids),
    {ok, {message, Res3, Ch3, _, _, _}} = west:send(Pid2, "r3", "M23"),
    ?assertEqual("sending_succeeded", Res3),
    ?assertEqual("r3", Ch3),
    timer:sleep(100),
    [{_, M23}] = ets:lookup(west_test, west_utils:build_name(["u2", "M23"])),
    ?assertEqual("M23", M23),
    %% send msg from Pid3 to 'r1' channel
    Pid3 = west_utils:keyfind(3, Pids),
    {ok, {message, Res4, Ch4, _, _, _}} = west:send(Pid3, "r1", "M31"),
    ?assertEqual("sending_succeeded", Res4),
    ?assertEqual("r1", Ch4),
    timer:sleep(100),
    [{_, M31}] = ets:lookup(west_test, west_utils:build_name(["u3", "M31"])),
    ?assertEqual("M31", M31),
    %% send msg from Pid3 to 'r2' channel
    {ok, {message, Res5, Ch5, _, _, _}} = west:send(Pid3, "r2", "M32"),
    ?assertEqual("sending_succeeded", Res5),
    ?assertEqual("r2", Ch5),
    timer:sleep(100),
    [{_, M32}] = ets:lookup(west_test, west_utils:build_name(["u3", "M32"])),
    ?assertEqual("M32", M32).

t_west_unreg({Pids, _}) ->
    %% unregister Pid1
    Pid1 = west_utils:keyfind(1, Pids),
    {ok, {message, Res0, Ch0, _, _, _}} = west:unreg(Pid1, "r1"),
    ?assertEqual("unregistration_succeeded", Res0),
    ?assertEqual("r1", Ch0),
    {ok, {message, Res1, Ch1, _, _, _}} = west:unreg(Pid1, "r1"),
    ?assertEqual("registration_not_found", Res1),
    ?assertEqual("r1", Ch1),
    %% send msg from Pid1 to 'r1' channel
    {ok, {message, Res2, Ch2, _, _, _}} = west:send(Pid1, "r1", "M111"),
    ?assertEqual("registration_not_found", Res2),
    ?assertEqual("r1", Ch2),
    %% send msg from Pid2 to 'r1' channel
    Pid2 = west_utils:keyfind(2, Pids),
    {ok, {message, Res3, Ch3, _, _, _}} = west:send(Pid2, "r1", "M211"),
    ?assertEqual("registration_not_found", Res3),
    ?assertEqual("r1", Ch3),
    %% send msg from Pid3 to 'r1' channel
    Pid3 = west_utils:keyfind(3, Pids),
    {ok, {message, Res4, Ch4, _, _, _}} = west:send(Pid3, "r1", "M311"),
    ?assertEqual("registration_not_found", Res4),
    ?assertEqual("r1", Ch4),
    %% send msg from Pid1 to 'r2' channel
    {ok, {message, Res5, Ch5, _, _, _}} = west:send(Pid1, "r2", "M121"),
    ?assertEqual("sending_succeeded", Res5),
    ?assertEqual("r2", Ch5),
    timer:sleep(100),
    [{_, M121}] = ets:lookup(west_test, west_utils:build_name(["u1", "M121"])),
    ?assertEqual("M121", M121),
    %% send msg from Pid2 to 'r3' channel
    {ok, {message, Res6, Ch6, _, _, _}} = west:send(Pid2, "r3", "M231"),
    ?assertEqual("sending_succeeded", Res6),
    ?assertEqual("r3", Ch6),
    timer:sleep(100),
    [{_, M231}] = ets:lookup(west_test, west_utils:build_name(["u2", "M231"])),
    ?assertEqual("M231", M231),
    %% unregister Pid2 from 'r3'
    {ok, {message, Res7, Ch7, _, _, _}} = west:unreg(Pid2, "r3"),
    ?assertEqual("registration_not_found", Res7),
    ?assertEqual("r3", Ch7).

t_west_sub({Pids, _}) ->
    %% sub Pid1
    Pid1 = west_utils:keyfind(1, Pids),
    {ok, {message, Res0, Ch0, _, _, _}} = west:sub(Pid1, "ps1"),
    ?assertEqual("subscription_succeeded", Res0),
    ?assertEqual("ps1", Ch0),
    {ok, {message, Res1, Ch1, _, _, _}} = west:sub(Pid1, "ps1"),
    ?assertEqual("subscription_already_exist", Res1),
    ?assertEqual("ps1", Ch1),
    %% sub Pid2
    Pid2 = west_utils:keyfind(2, Pids),
    {ok, {message, Res2, Ch2, _, _, _}} = west:sub(Pid2, "ps1"),
    ?assertEqual("subscription_succeeded", Res2),
    ?assertEqual("ps1", Ch2),
    {ok, {message, Res3, Ch3, _, _, _}} = west:sub(Pid2, "ps2"),
    ?assertEqual("subscription_succeeded", Res3),
    ?assertEqual("ps2", Ch3),
    %% sub Pid3
    Pid3 = west_utils:keyfind(3, Pids),
    {ok, {message, Res4, Ch4, _, _, _}} = west:sub(Pid3, "ps1"),
    ?assertEqual("subscription_succeeded", Res4),
    ?assertEqual("ps1", Ch4),
    {ok, {message, Res5, Ch5, _, _, _}} = west:sub(Pid3, "ps2"),
    ?assertEqual("subscription_succeeded", Res5),
    ?assertEqual("ps2", Ch5),
    {ok, {message, Res6, Ch6, _, _, _}} = west:sub(Pid3, "ps3"),
    ?assertEqual("subscription_succeeded", Res6),
    ?assertEqual("ps3", Ch6).

t_west_pub({Pids, _}) ->
    %% pub msg from Pid1 to 'ps1' channel
    Pid1 = west_utils:keyfind(1, Pids),
    {ok, {message, Res0, Ch0, _, _, _}} = west:pub(Pid1, "ps1", "U1PS1"),
    ?assertEqual("publication_succeeded", Res0),
    ?assertEqual("ps1", Ch0),
    timer:sleep(100),
    ?assertEqual(3, length(ets:lookup(west_test, west_utils:build_name(["u1", "U1PS1"])))),
    %% pub msg from Pid1 to 'ps2' channel
    {ok, {message, Res1, Ch1, _, _, _}} = west:pub(Pid1, "ps2", "U1PS2"),
    ?assertEqual("publication_succeeded", Res1),
    ?assertEqual("ps2", Ch1),
    timer:sleep(100),
    ?assertEqual(2, length(ets:lookup(west_test, west_utils:build_name(["u1", "U1PS2"])))),
    %% pub msg from Pid2 to 'ps3' channel
    Pid2 = west_utils:keyfind(2, Pids),
    {ok, {message, Res2, Ch2, _, _, _}} = west:pub(Pid2, "ps3", "U2PS3"),
    ?assertEqual("publication_succeeded", Res2),
    ?assertEqual("ps3", Ch2),
    timer:sleep(100),
    [{_, U2PS3}] = ets:lookup(west_test, west_utils:build_name(["u2", "U2PS3"])),
    ?assertEqual("U2PS3", U2PS3),
    %% pub msg from Pid2 to 'ps1' channel
    {ok, {message, Res3, Ch3, _, _, _}} = west:pub(Pid2, "ps1", "U2PS1"),
    ?assertEqual("publication_succeeded", Res3),
    ?assertEqual("ps1", Ch3),
    timer:sleep(100),
    ?assertEqual(3, length(ets:lookup(west_test, west_utils:build_name(["u2", "U2PS1"])))),
    %% pub msg from Pid3 to 'ps1' channel
    Pid3 = west_utils:keyfind(3, Pids),
    {ok, {message, Res4, Ch4, _, _, _}} = west:pub(Pid3, "ps1", "U3PS1"),
    ?assertEqual("publication_succeeded", Res4),
    ?assertEqual("ps1", Ch4),
    timer:sleep(100),
    ?assertEqual(3, length(ets:lookup(west_test, west_utils:build_name(["u3", "U3PS1"])))),
    %% pub msg from Pid1 to 'ps3' channel
    {ok, {message, Res5, Ch5, _, _, _}} = west:pub(Pid1, "ps3", "U1PS3"),
    ?assertEqual("publication_succeeded", Res5),
    ?assertEqual("ps3", Ch5),
    timer:sleep(100),
    [{_, U1PS3}] = ets:lookup(west_test, west_utils:build_name(["u1", "U1PS3"])),
    ?assertEqual("U1PS3", U1PS3),
    %% pub msg from Pid3 to 'ps2' channel
    {ok, {message, Res6, Ch6, _, _, _}} = west:pub(Pid3, "ps2", "U3PS2"),
    ?assertEqual("publication_succeeded", Res6),
    ?assertEqual("ps2", Ch6),
    timer:sleep(100),
    ?assertEqual(2, length(ets:lookup(west_test, west_utils:build_name(["u3", "U3PS2"])))).

t_west_unsub({Pids, _}) ->
    %% unsub Pid3 from 'ps1'
    Pid3 = west_utils:keyfind(3, Pids),
    {ok, {message, Res0, Ch0, _, _, _}} = west:unsub(Pid3, "ps1"),
    ?assertEqual("unsubscription_succeeded", Res0),
    ?assertEqual("ps1", Ch0),
    {ok, {message, Res1, Ch1, _, _, _}} = west:unsub(Pid3, "ps1"),
    ?assertEqual("subscription_not_found", Res1),
    ?assertEqual("ps1", Ch1),
    %% pub msg from Pid3 to 'ps1' channel
    {ok, {message, Res2, Ch2, _, _, _}} = west:pub(Pid3, "ps1", "U3PS11"),
    ?assertEqual("publication_succeeded", Res2),
    ?assertEqual("ps1", Ch2),
    timer:sleep(100),
    ?assertEqual(2, length(ets:lookup(west_test, west_utils:build_name(["u3", "U3PS11"])))),
    %% unsub Pid3 from 'ps2'
    {ok, {message, Res3, Ch3, _, _, _}} = west:unsub(Pid3, "ps2"),
    ?assertEqual("unsubscription_succeeded", Res3),
    ?assertEqual("ps2", Ch3),
    %% pub msg from Pid3 to 'ps2' channel
    {ok, {message, Res4, Ch4, _, _, _}} = west:pub(Pid3, "ps2", "U3PS21"),
    ?assertEqual("publication_succeeded", Res4),
    ?assertEqual("ps2", Ch4),
    timer:sleep(100),
    ?assertEqual(1, length(ets:lookup(west_test, west_utils:build_name(["u3", "U3PS21"])))),
    %% unsub Pid2 from 'ps1'
    Pid2 = west_utils:keyfind(2, Pids),
    {ok, {message, Res5, Ch5, _, _, _}} = west:unsub(Pid2, "ps1"),
    ?assertEqual("unsubscription_succeeded", Res5),
    ?assertEqual("ps1", Ch5),
    {ok, {message, Res6, Ch6, _, _, _}} = west:unsub(Pid2, "ps1"),
    ?assertEqual("subscription_not_found", Res6),
    ?assertEqual("ps1", Ch6),
    %% pub msg from Pid2 to 'ps1' channel
    {ok, {message, Res7, Ch7, _, _, _}} = west:pub(Pid2, "ps1", "U2PS11"),
    ?assertEqual("publication_succeeded", Res7),
    ?assertEqual("ps1", Ch7),
    timer:sleep(100),
    ?assertEqual(1, length(ets:lookup(west_test, west_utils:build_name(["u2", "U2PS11"])))).

t_west_pub2({Pids, _}) ->
    %% pub msg from Pid3 to 'ps1' channel
    Pid3 = west_utils:keyfind(3, Pids),
    {ok, {message, Res0, Ch0, _, _, _}} = west:pub(Pid3, "ps1", "U3PS12"),
    ?assertEqual("publication_succeeded", Res0),
    ?assertEqual("ps1", Ch0),
    timer:sleep(100),
    [{_, U3PS12}] = ets:lookup(west_test, west_utils:build_name(["u3", "U3PS12"])),
    ?assertEqual("U3PS12", U3PS12),
    %% pub msg from Pid1 to 'ps2' channel
    Pid1 = west_utils:keyfind(1, Pids),
    {ok, {message, Res1, Ch1, _, _, _}} = west:pub(Pid1, "ps2", "U1PS22"),
    ?assertEqual("publication_succeeded", Res1),
    ?assertEqual("ps2", Ch1),
    timer:sleep(100),
    [{_, U1PS22}] = ets:lookup(west_test, west_utils:build_name(["u1", "U1PS22"])),
    ?assertEqual("U1PS22", U1PS22),
    %% pub msg from Pid2 to 'ps3' channel
    Pid2 = west_utils:keyfind(2, Pids),
    {ok, {message, Res2, Ch2, _, _, _}} = west:pub(Pid2, "ps3", "U2PS32"),
    ?assertEqual("publication_succeeded", Res2),
    ?assertEqual("ps3", Ch2),
    timer:sleep(100),
    [{_, U2PS32}] = ets:lookup(west_test, west_utils:build_name(["u2", "U2PS32"])),
    ?assertEqual("U2PS32", U2PS32).

-endif.
