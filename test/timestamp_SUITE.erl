-module(timestamp_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([seconds/1]).
-export([milliseconds/1]).
-export([timestamp/1]).

%% ============================================================================
%% ct functions
%% ============================================================================

all() ->
    [
        seconds,
        milliseconds,
        timestamp
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

seconds(_) ->
    Question = <<"1624433430">>,
    ExpectedAnswer = #{
        question => Question,
        type => text,
        short => <<"2021-06-23 07:30:30 UTC">>
    },
    Answer = rinseweb_wiz_timestamp:answer(Question, [<<"1624433430">>]),
    ExpectedAnswer = Answer,
    ok.

milliseconds(_) ->
    Question = <<"1624433430000">>,
    ExpectedAnswer = #{
        question => Question,
        type => text,
        short => <<"2021-06-23 07:30:30 UTC">>
    },
    Answer = rinseweb_wiz_timestamp:answer(Question, [<<"1624433430">>]),
    ExpectedAnswer = Answer,
    ok.

timestamp(_) ->
    TestStartTime = erlang:system_time(second),
    Questions = [<<"now">>, <<"timestamp">>, <<"unix timestamp">>],
    F = fun (Question, Acc) ->
            Answer = rinseweb_wiz_timestamp:answer(Question, []),
            AnswerQuestion = maps:get(question, Answer),
            Question = AnswerQuestion,
            AnswerType = maps:get(type, Answer),
            text = AnswerType,
            AnswerTimeBin = maps:get(short, Answer),
            AnswerTimeInt = binary_to_integer(AnswerTimeBin),
            OneMinFromTestStart = TestStartTime + 60,
            true = AnswerTimeInt >= TestStartTime,
            true = AnswerTimeInt < OneMinFromTestStart,
            Acc
        end,
    ok = lists:foldr(F, ok, Questions).
