-module(base_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([max_question_length/1]).

%% ============================================================================
%% ct functions
%% ============================================================================

all() ->
    [
        max_question_length
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

max_question_length(_) ->
    QuestionLong = binary:copy(<<"a">>, 200),
    QuestionMax = binary:copy(<<"a">>, 128),
    ExpectedAnswer = #{
        question => QuestionMax,
        answers => []
    },
    Answer = rinseweb_wiz:answer(QuestionLong),
    ExpectedAnswer = Answer,
    ok.
