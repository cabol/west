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
-module(west_msg).

%% API
-export([dec_msg/2, enc_msg/2, build_msg/6]).

-include("west.hrl").
-include("west_int.hrl").

-type field() :: atom() | binary() | iolist().

%%%===================================================================
%%% API
%%%===================================================================

-spec dec_msg(any(), atom()) -> msg_spec().
dec_msg(Data, Encoding) ->
  dec_msg1(Data, Encoding).

-spec enc_msg(msg_spec(), atom()) -> any().
enc_msg(Msg, Encoding) ->
  enc_msg1(Msg, Encoding).

-spec build_msg(field(), field(), field(), field(), field(), atom()) -> any().
build_msg(Id, From, Event, Channel, Data, Encoding) ->
  Msg = ?MSG{id = Id, from = From, event = Event, channel = Channel, data = Data},
  enc_msg(Msg, Encoding).

%%%===================================================================
%%% Internals
%%%===================================================================

%% @private
dec_msg1(Data, json) ->
  case ?DEC_JSON(Data) of
    {error, _} ->
      {error, invalid_json};
    {DecJson} ->
      try
        Id = west_util:keyfind(<<"id">>, DecJson),
        From = west_util:keyfind(<<"from">>, DecJson),
        Event = west_util:to_atom(west_util:keyfind(<<"event">>, DecJson)),
        Ch = west_util:keyfind(<<"channel">>, DecJson),
        Data = west_util:keyfind(<<"data">>, DecJson),
        ?MSG{event = Event, channel = Ch, from = From, id = Id, data = Data}
      catch
        _:_ -> {error, decoding_error}
      end
  end;
dec_msg1(Data, _) ->
  Data.

%% @private
enc_msg1(Msg, json) ->
  F = ?record_to_list(msg_t),
  ?ENC_JSON({F(Msg)});
enc_msg1(Msg, _) ->
  Msg.
