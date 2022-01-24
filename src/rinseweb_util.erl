%%%-------------------------------------------------------------------
%% @doc rinseweb util
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_util).

%% API
-export([url_encode/1]).

%% Types
-type encode_string() :: binary() | string().

%%====================================================================
%% API
%%====================================================================

-spec url_encode(encode_string()) -> encode_string().
url_encode(Str) when is_list(Str) ->
    url_encode_string(Str);
url_encode(Str) when is_binary(Str) ->
    list_to_binary(url_encode_string(binary_to_list(Str))).

%%====================================================================
%% Internal functions
%%====================================================================

-spec url_encode_string(string()) -> string().
url_encode_string(Str) ->
    edoc_lib:escape_uri(Str).
