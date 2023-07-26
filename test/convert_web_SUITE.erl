-module(convert_web_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([convert_json/1]).
-export([convert_json_extra_whitespace/1]).
-export([convert_json_case_insensitive/1]).
-export([convert_unrecognized_syntax/1]).
-export([convert_unsupported_unit/1]).
-export([convert_invalid_number/1]).
-export([convert_use_cache/1]).

%% ============================================================================
%% ct functions
%% ============================================================================

all() ->
    [
        convert_json,
        convert_json_extra_whitespace,
        convert_json_case_insensitive,
        convert_unrecognized_syntax,
        convert_unsupported_unit,
        convert_invalid_number,
        convert_use_cache
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(rinseweb),
    Config.

end_per_suite(_) ->
    ok = application:stop(rinseweb).

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%% ============================================================================
%% Helpers
%% ============================================================================

expected_answer(UnitFromNum, UnitFrom, UnitToNum, UnitTo) ->
    #{
        <<"source">> => <<"convert">>,
        <<"type">> => <<"conversion_result">>,
        <<"answer">> => #{
            <<"unit_from_name">> => UnitFrom,
            <<"unit_from_number">> => UnitFromNum,
            <<"unit_to_name">> => UnitTo,
            <<"unit_to_number">> => UnitToNum
        }
    }.

expected_answer_shrug_default() ->
    expected_answer_shrug(<<"Unrecognized command">>, <<"default">>).

expected_answer_shrug(Reason, Source) ->
    #{
        <<"answer">> => Reason,
        <<"source">> => Source,
        <<"type">> => <<"shrug">>
    }.

%% ============================================================================
%% Tests
%% ============================================================================

convert_json(_) ->
    Answer = rinseweb_test:request_and_decode_answer("convert 20 km to miles"),
    ExpectedAnswer = expected_answer(20, <<"km">>, 12.427424, <<"miles">>),
    ExpectedAnswer = Answer,
    ok.

convert_json_extra_whitespace(_) ->
    Answer = rinseweb_test:request_and_decode_answer("   convert 20 km to miles  "),
    ExpectedAnswer = expected_answer(20, <<"km">>, 12.427424, <<"miles">>),
    ExpectedAnswer = Answer,
    ok.

convert_json_case_insensitive(_) ->
    Answer = rinseweb_test:request_and_decode_answer("ConVerT 20 km to miles"),
    ExpectedAnswer = expected_answer(20, <<"km">>, 12.427424, <<"miles">>),
    ExpectedAnswer = Answer,
    ok.

convert_unrecognized_syntax(_) ->
    Answer = rinseweb_test:request_and_decode_answer("convert foobar"),
    ExpectedAnswer = expected_answer_shrug_default(),
    ExpectedAnswer = Answer,
    ok.

convert_unsupported_unit(_) ->
    Answer = rinseweb_test:request_and_decode_answer("convert 5 jujumeters to meters"),
    ExpectedAnswer = expected_answer_shrug(<<"Unknown unit 'jujumeters'">>, <<"convert">>),
    ExpectedAnswer = Answer,
    ok.

convert_invalid_number(_) ->
    TestCases = [
        "", ".", ".1.", "..1", "1..",
        "-", "-.", "-.1.", "-..1", "-1.."
    ],
    F = fun(Num, Acc) ->
            Question = "convert " ++ Num ++ " kg to pounds",
            Answer = rinseweb_test:request_and_decode_answer(Question),
            ExpectedAnswer = expected_answer_shrug_default(),
            ExpectedAnswer = Answer,
            Acc
        end,
    ok = lists:foldr(F, ok, TestCases).

convert_use_cache(_) ->
    Question = "convert 20 km to m",
    QuestionBin = list_to_binary(Question),
    undefined = rinseweb_cache:get(rinseweb_wiz_convert, QuestionBin),
    Answer1 = rinseweb_test:request_and_decode_answer(Question),
    % overwrite cache to make sure we are reading from it the 2nd time
    ok = rinseweb_cache:put(rinseweb_wiz_convert, QuestionBin, #{foo => bar, type => baz}),
    Answer2 = rinseweb_test:request_and_decode_answer(Question),
    ExpectedAnswer1 = expected_answer(20, <<"km">>, 20000, <<"m">>),
    ExpectedAnswer2 = #{<<"foo">> => <<"bar">>, <<"type">> => <<"baz">>},
    ExpectedAnswer1 = Answer1,
    ExpectedAnswer2 = Answer2,
    ok.
