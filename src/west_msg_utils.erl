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
%%% @doc Common JSON utilities to `WEST'.
%%% @end
%%% Created : 03. Oct 2013 6:13 PM
%%%-------------------------------------------------------------------
-module(west_msg_utils).

%% API
-export([parse_msg/1, format_msg/1, build_msg/6]).

-include("west.hrl").
-include("west_int.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Parse the `Json' representation of a `?MSG' and return the
%%      parsed `?MSG' record.
%% @spec parse_msg(Json :: iodata()) -> Reply :: msg_spec()
parse_msg(Json) ->
    case ?DEC_JSON(Json) of
        {error, _} ->
            {error, <<"Invalid JSON, error at decode.">>};
        {DecJson} ->
            try
                Event = west_utils:keyfind(<<"event">>, DecJson),
                Ch = west_utils:keyfind(<<"channel">>, DecJson),
                From = west_utils:keyfind(<<"from">>, DecJson),
                Id = west_utils:keyfind(<<"id">>, DecJson),
                Data = west_utils:keyfind(<<"data">>, DecJson),
                ?MSG{event=Event, channel=Ch, from=From, id=Id, data=Data}
            catch
                _:_ -> {error, <<"Parsing error.">>}
            end
    end.

%% @doc Formats the given message `Msg' and return the Json
%%      representation as iodata.
%% @spec format_msg(Msg :: msg_spec()) -> Reply :: iodata()
format_msg(Msg) ->
    ?MSG{event=Ev, channel=Ch, from=From, id=Id, data=Data} = Msg,
    L0 = [{event, Ev}, {channel, Ch}, {from, From}, {id, Id}, {data, Data}],
    F = fun(X) when is_atom(X) -> atom_to_binary(X, utf8);
           (X) when is_list(X) orelse is_binary(X) -> iolist_to_binary(X);
           (X) -> iolist_to_binary(lists:flatten(io_lib:format("~p", [X])))
        end,
    L = [{F(X), F(Y)} || {X, Y} <- L0, Y =/= undefined],
    ?ENC_JSON({L}).

%% @doc Build a `?MSG' with the given arguments, and format that
%%      message and return a Json as iodata if `Format' is json.
%% @spec build_msg(Id, From, Event, Channel, Data, Format) -> Reply :: iodata()
%% Id = iolist() | undefined
%% From = iolist() | undefined
%% Event = iolist() | undefined
%% Channel = iolist() | undefined
%% Data = iolist() | undefined
%% Format = atom()
build_msg(Id, From, Event, Channel, Data, Format) ->
    Msg = ?MSG{event=Event, channel=Channel, from=From, id=Id, data=Data},
    case Format of
        json ->
            case format_msg(Msg) of
                {error, _} ->
                    <<"{\"from\":\"west\", \"event\":\"internal_error\"}">>;
                Enc ->
                    Enc
            end;
        _ ->
            Msg
    end.
