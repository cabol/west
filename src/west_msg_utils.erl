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
%%% @doc Common JSON utilities to `WEST'.
%%% @end
%%% Created : 03. Oct 2013 6:13 PM
%%%-------------------------------------------------------------------
-module(west_msg_utils).

%% API
-export([parse_msg/1, format_msg/1, build_msg/6, build_msg/7]).

-include("west.hrl").
-include("west_int.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @spec parse_msg(Json :: binary()) -> Reply :: msg_spec()
%%
%% @doc
%% Parse the `Json' representation of a `?MSG' and return the parsed
%% `?MSG' record.
%%
%% @end
%%--------------------------------------------------------------------
parse_msg(Json) ->
    case ?DEC_JSON(Json) of
        {fail, _} ->
            {error, <<"Invalid JSON, error at decode.">>};
        {struct, DecJson} ->
            try
                Id = proplists:get_value(<<"id">>, DecJson),
                From = proplists:get_value(<<"from">>, DecJson),
                Cmd = proplists:get_value(<<"command">>, DecJson),
                Event = proplists:get_value(<<"event">>, DecJson),
                Ch = proplists:get_value(<<"channel">>, DecJson),
                Data = proplists:get_value(<<"data">>, DecJson),
                ?MSG{id=Id,
                     from=From,
                     command=Cmd,
                     event=Event,
                     channel=Ch,
                     data=Data}
            catch
                _:_ -> {error, <<"Parsing error.">>}
            end
    end.

%%--------------------------------------------------------------------
%% @spec format_msg(Msg :: msg_spec()) -> Reply :: binary()
%%
%% @doc
%% Formats the given message `Msg' and return the Json representation
%% as a binary.
%%
%% @end
%%--------------------------------------------------------------------
format_msg(Msg) ->
    ?MSG{id=Id, from=From, command=Cmd, event=Ev, channel=Ch, data=Data} = Msg,
    L0 = [{id, Id},
          {from, From},
          {command, Cmd},
          {event, Ev},
          {channel, Ch},
          {data, Data}],
    F = fun(X) when is_atom(X) -> atom_to_binary(X, utf8);
           (X) when is_list(X) orelse is_binary(X) -> iolist_to_binary(X);
           (X) -> iolist_to_binary(lists:flatten(io_lib:format("~p", [X])))
        end,
    L = [{F(X), F(Y)} || {X, Y} <- L0, Y =/= undefined],
    case ?ENC_JSON(L) of
        {fail, _} -> {error, <<"Invalid MSG, error at encode.">>};
        EncJson   -> EncJson
    end.

%%--------------------------------------------------------------------
%% @spec build_msg(Id, From, Event, Channel, Data, Format) ->
%%                 Reply :: binary()
%%
%% @doc
%% Call build_msg/7 with Cmd = undefined.
%%
%% @end
%%--------------------------------------------------------------------
build_msg(Id, From, Event, Channel, Data, Format) ->
    build_msg(Id, From, undefined, Event, Channel, Data, Format).

%%--------------------------------------------------------------------
%% @spec build_msg(Id, From, Cmd, Event, Channel, Data, Format) ->
%%                 Reply :: binary()
%%
%% Types:
%%    Id = iolist() | undefined
%%    From = iolist() | undefined
%%    Cmd = iolist() | undefined
%%    Event = iolist() | undefined
%%    Channel = iolist() | undefined
%%    Data = iolist() | undefined
%%    Format = atom()
%%
%% @doc
%% Build a `?MSG' with the given arguments, and format that message
%% and return a Json as binary if `Format' is json.
%%
%% @end
%%--------------------------------------------------------------------
build_msg(Id, From, Cmd, Event, Channel, Data, Format) ->
    Msg = ?MSG{id=Id,
               from=From,
               command=Cmd,
               event=Event,
               channel=Channel,
               data=Data},
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
