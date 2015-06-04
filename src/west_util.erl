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
%%% @doc Common utilities.
%%% @end
%%% Created : 07. Oct 2013 9:30 PM
%%%-------------------------------------------------------------------
-module(west_util).

%% API
-export([keyfind/2, keyfind/3, parse_query_string/1,
        get_prop/3, props_for_types/2, format_datetime/2,
        curr_date/0, parse_datetime/1, get_timestamp_ms/0,
        parse_timestamp_ms/1, hash_string/2, bin_to_hex/1,
        hmac/3, build_name/1, random_hash/1,
        random_string/1, strong_rand/1, next_id/1,
        to_bin/1, to_atom/1, to_integer/1, to_float/1,
        start_app_deps/1, dec_json/1, enc_json/1]).


%% Types
-type tuple_list() :: [{any(), any()}].
-type json_term()  :: [json_term()] | {[json_term()]} |
                      [{binary() | atom() | integer(), json_term()}] |
                      integer() | float() | binary() | atom().

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Calls keyfind/3 with Default = undefined.
-spec keyfind(any(), tuple_list()) -> term().
keyfind(Key, TupleList) ->
  keyfind(Key, TupleList, undefined).

%% @doc Searches the list of tuples TupleList for a tuple whose Nth element
%%      compares equal to Key. Returns Tuple's value if such a tuple is
%%      found, otherwise Default.
-spec keyfind(any(), tuple_list(), any()) -> term().
keyfind(Key, TupleList, Default) ->
  case lists:keyfind(Key, 1, TupleList) of
    {_K, V} -> V;
    _ -> Default
  end.

%% @doc Parse a given query string and returns a key/value pair list.
-spec parse_query_string(string()) -> [{string(), string()}] | error.
parse_query_string(Str) ->
  F = fun(Q) ->
        [{K, V} || [K, V] <- [string:tokens(L, "=") || L <- string:tokens(Q, "&")]]
      end,
  case string:tokens(Str, "?") of
    [_, X] -> F(X);
    [X] -> F(X);
    _ -> []
  end.

%% @doc Gets a property given by Name and if it doesn't exist, returns Default.
-spec get_prop(string(), list(), string()) -> term().
get_prop(Name, Props, Default) ->
  case lists:keyfind(Name, 1, Props) of
    {_K, V} -> V;
    _ -> Default
  end.

%% @doc Filter all values in Props that match with some value in Types.
-spec props_for_types(list(), list()) -> list().
props_for_types(Types, Props) ->
  Fun =
    fun(Type, Acc) ->
      case lists:keyfind(Type, 1, Props) of
        {_, Param} -> [{Type, Param}] ++ Acc;
        _ -> Acc
      end
    end,
  lists:foldl(Fun, [], Types).

%% @doc Format the given DateTime with the format specified in the atom().
-spec format_datetime(atom(), calendar:datetime()) -> string().
format_datetime(iso8601, DateTime) ->
  {{Year, Month, Day}, {Hour, Min, Sec}} = DateTime,
  lists:flatten(io_lib:format(
    "~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
    [Year, Month, Day, Hour, Min, Sec]));
format_datetime(yyyymmdd, DateTime) ->
  {{Year, Month, Day}, _} = DateTime,
  lists:flatten(io_lib:format("~4.10.0B~2.10.0B~2.10.0B", [Year, Month, Day]));
format_datetime(rfc1123, DateTime) ->
  httpd_util:rfc1123_date(DateTime).

%% @doc Return current datetime in RFC-1123 format.
-spec curr_date() -> iolist().
curr_date() ->
  iolist_to_binary(format_datetime(rfc1123, calendar:local_time())).

%% @doc Converts a date string into datetime() format.
-spec parse_datetime(string()) -> term().
parse_datetime(DateStr) ->
  httpd_util:convert_request_date(DateStr).

%% @doc Returns a timestamp in milliseconds.
-spec get_timestamp_ms() -> integer().
get_timestamp_ms() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega * 1000000 + Sec) * 1000000 + Micro.

%% @doc Parse a given ms timestamp to os:timestamp() format.
-spec parse_timestamp_ms(integer()) -> {integer(), integer(), integer()}.
parse_timestamp_ms(Timestamp) ->
  {Timestamp div 1000000000000,
   Timestamp div 1000000 rem 1000000,
   Timestamp rem 1000000}.

%% @doc Hash the given data and return the hex-string result.
-spec hash_string(atom(), iodata()) -> string().
hash_string(Hash, Data) ->
  bin_to_hex(crypto:hash(Hash, Data)).

%% @doc Converts the given binary to hex string.
%% @see [hd(integer_to_list(Nibble, 16)) || <<Nibble:4>> <= B]
-spec bin_to_hex(binary()) -> string().
bin_to_hex(B) when is_binary(B) ->
  bin_to_hex(B, []).

%% @doc Wrap original hmac/3 from erlang crypto module, and converts it
%%      result into hex string to return it.
%% @see Erlang crypto:hmac(Type, Key, Data).
-spec hmac(atom(), iodata(), iodata()) -> string().
hmac(Type, Key, Data) ->
  bin_to_hex(crypto:hmac(Type, Key, Data)).

%% @doc Hash the given list and return an atom representation of that hash.
-spec build_name([any()]) -> atom().
build_name(L) when is_list(L) ->
  F = fun(X, Acc) -> <<Acc/binary, (<<"_">>)/binary, (to_bin(X))/binary>> end,
  Suffix = lists:foldl(F, <<"">>, L),
  binary_to_atom(<<(<<"p">>)/binary, Suffix/binary>>, utf8).

%% @doc Generates a random hash hex-string.
-spec random_hash(atom()) -> string().
random_hash(Hash) ->
  Ts = lists:flatten(io_lib:format("~p", [erlang:phash2(os:timestamp())])),
  string:to_upper(bin_to_hex(crypto:hash(Hash, Ts))).

%% @doc Generates a random string.
-spec random_string(integer()) -> string().
random_string(Len) ->
  <<A1:32, A2:32, A3:32>> = crypto:strong_rand_bytes(12),
  random:seed({A1, A2, A3}),
  Chrs = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
  %%Chrs = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"),
  ChrsSize = size(Chrs),
  F = fun(_, R) -> [element(random:uniform(ChrsSize), Chrs) | R] end,
  lists:foldl(F, "", lists:seq(1, Len)).

%% @doc Generates N bytes randomly uniform 0..255, and returns result encoded
%%      Base64. Uses a cryptographically secure prng seeded and periodically
%%      mixed with operating system provided entropy. By default this is the
%%      RAND_bytes method from OpenSSL.
%%      May throw exception low_entropy in case the random generator failed
%%      due to lack of secure "randomness".
-spec strong_rand(integer()) -> iolist().
strong_rand(N) ->
  F = fun($/) -> $X; ($+) -> $Y; (D) -> D end,
  <<<<(F(D))>> || <<D>> <= base64:encode(crypto:strong_rand_bytes(N)), D =/= $=>>.

%% @doc Generates an unique ID.
-spec next_id(iolist()|binary()) -> iolist() | binary().
next_id(Prefix) ->
  <<Prefix/binary, (iolist_to_binary(random_string(32)))/binary>>.

%% @doc Converts any type to binary.
-spec to_bin(any()) -> atom().
to_bin(Data) when is_integer(Data) ->
  integer_to_binary(Data);
to_bin(Data) when is_float(Data) ->
  float_to_binary(Data);
to_bin(Data) when is_atom(Data) ->
  atom_to_binary(Data, utf8);
to_bin(Data) when is_list(Data) ->
  iolist_to_binary(Data);
to_bin(Data) when is_pid(Data); is_reference(Data); is_tuple(Data) ->
  integer_to_binary(erlang:phash2(Data));
to_bin(Data) ->
  Data.

%% @doc Converts any type to atom.
-spec to_atom(any()) -> atom().
to_atom(Data) when is_binary(Data) ->
  binary_to_atom(Data, utf8);
to_atom(Data) when is_list(Data) ->
  list_to_atom(Data);
to_atom(Data) when is_pid(Data); is_reference(Data); is_tuple(Data) ->
  list_to_atom(integer_to_list(erlang:phash2(Data)));
to_atom(Data) ->
  Data.

%% @doc Converts any type to integer.
-spec to_integer(any()) -> integer().
to_integer(Data) when is_binary(Data) ->
  binary_to_integer(Data);
to_integer(Data) when is_list(Data) ->
  list_to_integer(Data);
to_integer(Data) when is_pid(Data); is_reference(Data); is_tuple(Data) ->
  erlang:phash2(Data);
to_integer(Data) ->
  Data.

%% @doc Converts any type to float.
-spec to_float(any()) -> float().
to_float(Data) when is_binary(Data) ->
  binary_to_float(Data);
to_float(Data) when is_list(Data) ->
  list_to_float(Data);
to_float(Data) when is_pid(Data); is_reference(Data); is_tuple(Data) ->
  erlang:phash2(Data);
to_float(Data) ->
  Data.

%% @doc Starts the given application, starting recursively all its
%%      application dependencies. Returns a list with all started
%%      applications.
-spec start_app_deps(App :: atom()) -> StartedApps :: list().
start_app_deps(App) ->
  case application:start(App) of
    {error, {not_started, Dep}} ->
      start_app_deps([Dep | [App]], []);
    {error, {Reason, _}} ->
      [{Reason, App}];
    ok ->
      [{ok, App}]
  end.
start_app_deps([], Acc) ->
  Acc;
start_app_deps([H | T] = L, Acc) ->
  case application:start(H) of
    {error, {not_started, Dep}} ->
      start_app_deps([Dep | L], Acc);
    {error, {Reason, _}} ->
      start_app_deps(T, [{Reason, H} | Acc]);
    ok ->
      start_app_deps(T, [{ok, H} | Acc])
  end.

%% @doc Decode a JSON iodata into JSON term.
-spec dec_json(iolist()) -> json_term().
dec_json(Json) ->
  try
    jiffy:decode(Json)
  catch
    _:_ -> {error, invalid_json}
  end.

%% @doc Encode a JSON term into a JSON IOstring.
-spec enc_json(json_term()) -> iolist().
enc_json(JsonTerm) ->
  try
    jiffy:encode(JsonTerm)
  catch
    _:_ -> {error, invalid_json_term}
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
