-module(dicrionaryapi_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([query/1]).

%% ============================================================================
%% ct functions
%% ============================================================================

all() ->
    [
        query
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

query(_) ->
    Question = <<"define hello">>,
    Answer = rinseweb_wiz_dictionaryapi:answer(Question, [<<"hello">>], #{}),
    definition = maps:get(type, Answer),
    definition = maps:get(source, Answer),
    AnswerCustom = maps:get(answer, Answer),
    true = is_list(AnswerCustom),
    true = length(AnswerCustom) > 0,
    [FirstItem|_] = AnswerCustom,
    true = maps:is_key(word, FirstItem),
    true = maps:is_key(phonetics, FirstItem),
    true = maps:is_key(meanings, FirstItem).
