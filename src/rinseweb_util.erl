%%%-------------------------------------------------------------------
%% @doc rinseweb util
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_util).

%% API
-export([binary_trim/1]).
-export([url_encode/1]).
-export([round_precise/1]).
-export([string_to_number/1]).
-export([number_to_binary/1]).
-export([binary_to_number/1]).

%% Types
-type encode_string() :: binary() | string().
-type httpc_status_line() :: {uri_string:uri_string(), non_neg_integer(), string()}.
-type httpc_http_header() :: {[byte()], binary() | iolist()}.

-export_type([httpc_status_line/0]).
-export_type([httpc_http_header/0]).

%%====================================================================
%% API
%%====================================================================

-spec binary_trim(binary()) -> binary().
binary_trim(Bin) ->
    binary_ltrim(binary_rtrim(Bin)).

-spec url_encode(encode_string()) -> encode_string().
url_encode(Str) when is_list(Str) ->
    url_encode_string(Str);
url_encode(Str) when is_binary(Str) ->
    list_to_binary(url_encode_string(binary_to_list(Str))).

-spec round_precise(number()) -> number().
round_precise(Num) when is_integer(Num) -> Num;
round_precise(Num) when is_float(Num) ->
    if
        round(Num) == Num -> round(Num);
        true -> Num
    end.

-spec string_to_number(string()) -> number() | undefined.
string_to_number([]) -> undefined;
string_to_number(S) ->
    case string:to_float(make_sci_notation_use_floats(S)) of
        {error, no_float} ->
            list_to_integer(S);
        {F, _Rest} -> F
    end.

-spec number_to_binary(number()) -> binary().
number_to_binary(Num) when is_integer(Num) -> integer_to_binary(Num);
number_to_binary(Num) when is_float(Num) -> float_to_binary(Num, [{decimals, 10}, compact]).

-spec binary_to_number(binary()) -> float() | integer().
binary_to_number(<<".", Rest/binary>>) -> binary_to_number(<<"0.", Rest/binary>>);
binary_to_number(<<"-.", Rest/binary>>) -> binary_to_number(<<"-0.", Rest/binary>>);
binary_to_number(B) ->
    try binary_to_float(B)
    catch
        error:badarg -> binary_to_integer(B)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec binary_ltrim(binary()) -> binary().
binary_ltrim(<<32, Bin/binary>>) -> binary_ltrim(Bin);
binary_ltrim(Bin) -> Bin.

-spec binary_rtrim(binary()) -> binary().
binary_rtrim(Bin) when binary_part(Bin, {byte_size(Bin), -1}) =:= <<32>> ->
    binary_rtrim(binary_part(Bin, {0, byte_size(Bin) - 1}));
binary_rtrim(Bin) -> Bin.

-spec url_encode_string(string()) -> string().
url_encode_string(Str) ->
    edoc_lib:escape_uri(Str).

%% @doc converts 1e+15 to 1.0e+15
-spec make_sci_notation_use_floats(string()) -> string().
make_sci_notation_use_floats(Str) ->
    fix_sci_notation(Str, string:split(Str, "e+")).

-spec fix_sci_notation(string(), [string()]) -> string().
fix_sci_notation(Orig, [Orig]) -> Orig;
fix_sci_notation(Orig, [Left, Right]) ->
    case is_string_integer(Left) of
        true -> Left ++ ".0" ++ "e+" ++ Right;
        false -> Orig
    end.

-spec is_string_integer(string()) -> boolean().
is_string_integer(Str) ->
    try
        _ = list_to_integer(Str),
        true
    catch error:badarg ->
        false
    end.
