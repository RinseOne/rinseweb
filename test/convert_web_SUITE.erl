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

result_shrug(Question) when is_list(Question) ->
    result_shrug(Question, <<"Unrecognized command">>, <<"default">>).

result_shrug(Question, Reason, Source) when is_list(Question) ->
    Shrug = #{
        <<"answer">> => Reason,
        <<"source">> => Source,
        <<"type">> => <<"shrug">>
    },
    result_custom_answers(Question, [Shrug]).

result_custom_answers(Question, Answers) ->
    #{
        <<"question">> => list_to_binary(Question),
        <<"answers">> => Answers
    }.

result(Question, UnitFromNum, UnitFrom, UnitToNum, UnitTo) ->
    #{
        <<"question">> => list_to_binary(Question),
        <<"answers">> => [
            #{
                <<"source">> => <<"convert">>,
                <<"type">> => <<"conversion_result">>,
                <<"answer">> => #{
                    <<"unit_from_name">> => UnitFrom,
                    <<"unit_from_number">> => UnitFromNum,
                    <<"unit_to_name">> => UnitTo,
                    <<"unit_to_number">> => UnitToNum
                }
            }
        ]
    }.

%% ============================================================================
%% Tests
%% ============================================================================

convert_json(_) ->
    Question = "convert 20 km to miles",
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, ResponseBody}} = rinseweb_test:request_json(Question),
    Response = rinseweb_test:decode_response_body(ResponseBody),
    true = lists:member({"content-type","application/json"}, Headers),
    ExpectedResponse = result(Question, 20, <<"km">>, 12.427424, <<"miles">>),
    ExpectedResponse = Response,
    ok.

convert_json_extra_whitespace(_) ->
    Question = "   convert 20 km to miles  ",
    QuestionTrimmed = "convert 20 km to miles",
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, ResponseBody}} = rinseweb_test:request_json(Question),
    Response = rinseweb_test:decode_response_body(ResponseBody),
    true = lists:member({"content-type","application/json"}, Headers),
    ExpectedResponse = result(QuestionTrimmed, 20, <<"km">>, 12.427424, <<"miles">>),
    ExpectedResponse = Response,
    ok.

convert_json_case_insensitive(_) ->
    Question = "ConVerT 20 km to miles",
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, ResponseBody}} = rinseweb_test:request_json(Question),
    Response = rinseweb_test:decode_response_body(ResponseBody),
    true = lists:member({"content-type","application/json"}, Headers),
    ExpectedResponse = result(Question, 20, <<"km">>, 12.427424, <<"miles">>),
    ExpectedResponse = Response,
    ok.

convert_unrecognized_syntax(_) ->
    Question = "convert foobar",
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, ResponseBody}} = rinseweb_test:request_json(Question),
    Response = rinseweb_test:decode_response_body(ResponseBody),
    true = lists:member({"content-type","application/json"}, Headers),
    ExpectedResponse = result_shrug(Question),
    ExpectedResponse = Response,
    ok.

convert_unsupported_unit(_) ->
    Question = "convert 5 jujumeters to meters",
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, ResponseBody}} = rinseweb_test:request_json(Question),
    Response = rinseweb_test:decode_response_body(ResponseBody),
    true = lists:member({"content-type","application/json"}, Headers),
    ExpectedResponse = result_shrug(Question, <<"Unknown unit 'jujumeters'">>, <<"convert">>),
    ExpectedResponse = Response,
    ok.

convert_invalid_number(_) ->
    TestCases = [
        "", ".", ".1.", "..1", "1..",
        "-", "-.", "-.1.", "-..1", "-1.."
    ],
    F = fun(Num, Acc) ->
            Question = "convert " ++ Num ++ " kg to pounds",
            {ok, {{"HTTP/1.1", 200, "OK"}, Headers, ResponseBody}} = rinseweb_test:request_json(Question),
            Response = rinseweb_test:decode_response_body(ResponseBody),
            true = lists:member({"content-type","application/json"}, Headers),
            ExpectedResponse = result_shrug(Question),
            ExpectedResponse = Response,
            Acc
        end,
    ok = lists:foldr(F, ok, TestCases).

convert_use_cache(_) ->
    Question = "convert 20 km to m",
    QuestionBin = list_to_binary(Question),
    undefined = rinseweb_cache:get(rinseweb_wiz_convert, QuestionBin),
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers1, ResponseBody1}} = rinseweb_test:request_json(Question),
    % overwrite cache to make sure we are reading from it the 2nd time
    ok = rinseweb_cache:put(rinseweb_wiz_convert, QuestionBin, #{foo => bar}),
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers2, ResponseBody2}} = rinseweb_test:request_json(Question),
    Response1 = rinseweb_test:decode_response_body(ResponseBody1),
    Response2 = rinseweb_test:decode_response_body(ResponseBody2),
    true = lists:member({"content-type","application/json"}, Headers1),
    true = lists:member({"content-type","application/json"}, Headers2),
    ExpectedResponse1 = result(Question, 20, <<"km">>, 20000, <<"m">>),
    ExpectedResponse2 = result_custom_answers(Question, [#{<<"foo">> => <<"bar">>}]),
    ExpectedResponse1 = Response1,
    ExpectedResponse2 = Response2,
    ok.
