-module(apply_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([add/1]).
-export([subtract/1]).
-export([multiply/1]).
-export([divide/1]).
-export([unknown_operator/1]).

%% ============================================================================
%% ct functions
%% ============================================================================

all() ->
    [
        add,
        subtract,
        multiply,
        divide,
        unknown_operator
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

add(_) ->
    Question = <<"+ 1 2 3">>,
    Answer = rinseweb_wiz_apply:answer(Question, [<<"+">>, <<" 1 2 3">>, <<" 3">>], #{}),
    number = maps:get(type, Answer),
    apply = maps:get(source, Answer),
    6 = maps:get(number, maps:get(answer, Answer)).

subtract(_) ->
    Question = <<"- 1 2 3">>,
    Answer = rinseweb_wiz_apply:answer(Question, [<<"-">>, <<" 1 2 3">>, <<" 3">>], #{}),
    number = maps:get(type, Answer),
    apply = maps:get(source, Answer),
    -4 = maps:get(number, maps:get(answer, Answer)).

multiply(_) ->
    Question = <<"* 1 2 3">>,
    Answer = rinseweb_wiz_apply:answer(Question, [<<"*">>, <<" 1 2 3">>, <<" 3">>], #{}),
    number = maps:get(type, Answer),
    apply = maps:get(source, Answer),
    6 = maps:get(number , maps:get(answer, Answer)).

divide(_) ->
    Question = <<"/ 4 2 2 2">>,
    Answer = rinseweb_wiz_apply:answer(Question, [<<"/">>, <<" 4 2 2 2">>, <<" 2">>], #{}),
    number = maps:get(type, Answer),
    apply = maps:get(source, Answer),
    0.5 = maps:get(number, maps:get(answer, Answer)).

unknown_operator(_) ->
    Question = <<"^ 1 2 3">>,
    Answer = rinseweb_wiz_apply:answer(Question, [<<"^">>, <<" 1 2 3">>, <<" 3">>], #{}),
    shrug = maps:get(type, Answer),
    apply = maps:get(source, Answer),
    <<"Unknown operator">> = maps:get(answer, Answer).
