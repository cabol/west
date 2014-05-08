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
%%% @doc Common utilities.
%%% @end
%%% Created : 07. Oct 2013 9:30 PM
%%%-------------------------------------------------------------------
-module(west_utils).

%% API
-export([parse_query_string/1,
         get_prop/3,
         validate_json/1,
         props_for_types/2,
         format_datetime/2,
         parse_datetime/1,
         get_timestamp_ms/0,
         parse_timestamp_ms/1,
         bin_to_hex/1,
         hmac/3,
         build_name/1,
         iolist_to_atom/1,
         start_app_deps/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Parse a given query string and returns a key/value pair list.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_query_string(string()) -> [{string(), string()}] | error.
parse_query_string(Str) ->
    F = fun(Q) ->
            [{K, V} || [K, V] <-
             [string:tokens(L, "=") || L <- string:tokens(Q, "&")]]
        end,
    case string:tokens(Str, "?") of
        [_, X] -> F(X);
        [X]    -> F(X);
        _      -> error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Gets a property given by Name and if it doesn't exist, returns
%% Default.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_prop(string(), list(), string()) -> term().
get_prop(Name, Props, Default) ->
    case proplists:lookup(Name, Props) of
        {_, V} -> V;
        _      -> Default
    end.

%%--------------------------------------------------------------------
%% @doc
%% Validates a Json string using mochijson2.
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_json(iolist()) -> term().
validate_json(Json) ->
    try
        mochijson2:decode(Json)
    catch
        throw:invalid_utf8 ->
            {fail, "Invalid JSON: Illegal UTF-8 character"};
        error:Error ->
            {fail, "Invalid JSON: " ++ binary_to_list(
                list_to_binary(io_lib:format("~p", [Error])))}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Filter all values in Props that match with some value in Types.
%%
%% @end
%%--------------------------------------------------------------------
-spec props_for_types(list(), list()) -> list().
props_for_types(Types, Props) ->
    Fun =
        fun(Type, Acc) ->
            case proplists:lookup(Type, Props) of
                {_, Param} -> [{Type, Param}] ++ Acc;
                none       -> Acc
            end
        end,
    lists:foldl(Fun, [], Types).

%%--------------------------------------------------------------------
%% @doc
%% Format the given DateTime with the format specified in the atom().
%%
%% @end
%%--------------------------------------------------------------------
-spec format_datetime(atom(), calendar:datetime()) -> string().
format_datetime(iso8601, DateTime) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = DateTime,
    lists:flatten(
        io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
                      [Year, Month, Day, Hour, Min, Sec]));
format_datetime(yyyymmdd, DateTime) ->
    {{Year, Month, Day}, _} = DateTime,
    lists:flatten(io_lib:format("~4.10.0B~2.10.0B~2.10.0B",
                                [Year, Month, Day]));
format_datetime(rfc1123, DateTime) ->
    httpd_util:rfc1123_date(DateTime).

%%--------------------------------------------------------------------
%% @doc
%% Converts a date string into datetime() format.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_datetime(string()) -> term().
parse_datetime(DateStr) ->
    httpd_util:convert_request_date(DateStr).

%%--------------------------------------------------------------------
%% @doc
%% Returns a timestamp in milliseconds.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_timestamp_ms() -> integer().
get_timestamp_ms() ->
    {Mega, Sec, Micro} = erlang:now(),
    (Mega * 1000000 + Sec) * 1000000 + Micro.

%%--------------------------------------------------------------------
%% @doc
%% Parse a given ms timestamp to erlang:now() format.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_timestamp_ms(integer()) -> {integer(), integer(), integer()}.
parse_timestamp_ms(Timestamp) ->
    {Timestamp div 1000000000000,
     Timestamp div 1000000 rem 1000000,
     Timestamp rem 1000000}.

%%--------------------------------------------------------------------
%% @doc
%% Converts the given binary to hex string.
%%
%% @see [hd(integer_to_list(Nibble, 16)) || <<Nibble:4>> <= B]
%%
%% @end
%%--------------------------------------------------------------------
-spec bin_to_hex(binary()) -> string().
bin_to_hex(B) when is_binary(B) ->
    bin_to_hex(B, []).

%%--------------------------------------------------------------------
%% @doc
%% Wrap original hmac/3 from erlang crypto module, and converts it
%% result into hex string to return it.
%%
%% @see Erlang crypto:hmac(Type, Key, Data).
%%
%% @end
%%--------------------------------------------------------------------
-spec hmac(atom(), iodata(), iodata()) -> string().
hmac(Type, Key, Data) ->
    bin_to_hex(crypto:hmac(Type, Key, Data)).

%%--------------------------------------------------------------------
%% @doc
%% Hash the given list and return an atom representation of that hash.
%%
%% @end
%%--------------------------------------------------------------------
-spec build_name(list()) -> atom().
build_name(L) when is_list(L) ->
    binary_to_atom(<<(<<"p">>)/binary,
                     (integer_to_binary(erlang:phash2(L)))/binary>>, utf8).

%%--------------------------------------------------------------------
%% @doc
%% Converts an iolist to atom.
%%
%% @end
%%--------------------------------------------------------------------
-spec iolist_to_atom(iolist()) -> atom().
iolist_to_atom(IoList) when is_list(IoList); is_binary(IoList) ->
    case IoList of
        IoList when is_binary(IoList) ->
            binary_to_atom(IoList, utf8);
        IoList when is_list(IoList) ->
            list_to_atom(IoList);
        _ ->
            throw({error, "Invalid type."})
    end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the given application, starting recursively all its
%% application dependencies. Returns a list with all started
%% applications.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_app_deps(App :: atom()) -> StartedApps :: list().
start_app_deps(App) when is_atom(App) ->
    case application:start(App) of
        {error, {not_started, Dep}} ->
            start_app_deps([Dep|[App]], []);
        {error, {_, _}} ->
            [App];
        ok ->
            [App]
    end.
start_app_deps([]=_L, Acc) when is_list(_L) ->
    Acc;
start_app_deps([H|T]=L, Acc) when is_list(L) ->
    case application:start(H) of
        {error, {not_started, Dep}} ->
            start_app_deps([Dep|L], Acc);
        {error, {_, _}} ->
            start_app_deps(T, [H|Acc]);
        ok ->
            start_app_deps(T, [H|Acc])
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
hexdigit(C) when C >= 0, C =< 9 ->
    C + $0;
hexdigit(C) when C =< 15 ->
    C + $a -10.

%% @private
bin_to_hex(<<>>, Acc) ->
    lists:reverse(Acc);
bin_to_hex(<<C1:4, C2:4, Rest/binary>>, Acc) ->
    bin_to_hex(Rest, [hexdigit(C2), hexdigit(C1) | Acc]).
