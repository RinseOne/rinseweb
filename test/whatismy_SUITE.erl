-module(whatismy_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([ipv4/1]).
-export([ipv6/1]).
-export([user_agent/1]).

%% ============================================================================
%% ct functions
%% ============================================================================

all() ->
    [
        ipv4,
        ipv6,
        user_agent
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

ipv4(_) ->
    Question = <<"wimip">>,
    Answer = rinseweb_wiz_whatismy:answer(Question, [], #{client_ip => {127, 0, 0, 1}}),
    text = maps:get(type, Answer),
    wimip = maps:get(source, Answer),
    <<"127.0.0.1">> = maps:get(text, maps:get(answer, Answer)).

ipv6(_) ->
    Question = <<"wimip">>,
    Answer = rinseweb_wiz_whatismy:answer(Question, [], #{client_ip => {8193, 3512, 0, 0, 0, 0, 2, 1}}),
    text = maps:get(type, Answer),
    wimip = maps:get(source, Answer),
    <<"2001:db8::2:1">> = maps:get(text, maps:get(answer, Answer)).

user_agent(_) ->
    Question = <<"wimua">>,
    Answer = rinseweb_wiz_whatismy:answer(Question, [], #{user_agent => <<"foo bar baz">>}),
    text = maps:get(type, Answer),
    wimip = maps:get(source, Answer),
    <<"foo bar baz">> = maps:get(text, maps:get(answer, Answer)).
