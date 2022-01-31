-module(util_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([binary_trim/1]).
-export([url_encode/1]).
-export([round_precise/1]).
-export([string_to_number/1]).
-export([number_to_binary/1]).
-export([binary_to_number/1]).

%% ============================================================================
%% ct functions
%% ============================================================================

all() ->
    [
        binary_trim,
        url_encode,
        round_precise,
        string_to_number,
        number_to_binary,
        binary_to_number
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_) ->
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%% ============================================================================
%% Tests
%% ============================================================================

binary_trim(_) ->
    <<>> = rinseweb_util:binary_trim(<<>>),
    <<"foo">> = rinseweb_util:binary_trim(<<"foo">>),
    <<"foo">> = rinseweb_util:binary_trim(<<" foo">>),
    <<"foo">> = rinseweb_util:binary_trim(<<"foo ">>),
    <<"foo">> = rinseweb_util:binary_trim(<<" foo ">>),
    <<"foo bar">> = rinseweb_util:binary_trim(<<"    foo bar ">>).

url_encode(_) ->
    "foo" = rinseweb_util:url_encode("foo"),
    <<"foo">> = rinseweb_util:url_encode(<<"foo">>),
    "foo%3fbar%20baz" = rinseweb_util:url_encode("foo?bar baz"),
    <<"foo%3fbar%20baz">> = rinseweb_util:url_encode(<<"foo?bar baz">>).

round_precise(_) ->
    5 = rinseweb_util:round_precise(5),
    5 = rinseweb_util:round_precise(5.0),
    5.2 = rinseweb_util:round_precise(5.2),
    5.2 = rinseweb_util:round_precise(5.2000).

string_to_number(_) ->
    undefined = rinseweb_util:string_to_number([]),
    5 = rinseweb_util:string_to_number("5"),
    5.2 = rinseweb_util:string_to_number("5.2"),
    -5.2 = rinseweb_util:string_to_number("-5.2"),
    5.2 = rinseweb_util:string_to_number("5.20"),
    0.2 = rinseweb_util:string_to_number("0.2").

number_to_binary(_) ->
    <<"5">> = rinseweb_util:number_to_binary(5),
    <<"-5">> = rinseweb_util:number_to_binary(-5),
    <<"5.7">> = rinseweb_util:number_to_binary(5.7).

binary_to_number(_) ->
    5 = rinseweb_util:binary_to_number(<<"5">>),
    5.2 = rinseweb_util:binary_to_number(<<"5.2">>),
    -5.2 = rinseweb_util:binary_to_number(<<"-5.2">>),
    5.2 = rinseweb_util:binary_to_number(<<"5.20">>),
    0.2 = rinseweb_util:binary_to_number(<<"0.2">>),
    0.2 = rinseweb_util:binary_to_number(<<".2">>),
    -0.2 = rinseweb_util:binary_to_number(<<"-0.2">>),
    -0.2 = rinseweb_util:binary_to_number(<<"-.2">>).
