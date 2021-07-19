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
        convert_invalid_number
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

result_no_answers(Question) when is_list(Question) ->
    #{
        <<"question">> => list_to_binary(Question),
        <<"answers">> => []
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
    ExpectedResponse = result_no_answers(Question),
    ExpectedResponse = Response,
    ok.

convert_unsupported_unit(_) ->
    Question = "convert 5 jujumeters to meters",
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, ResponseBody}} = rinseweb_test:request_json(Question),
    Response = rinseweb_test:decode_response_body(ResponseBody),
    true = lists:member({"content-type","application/json"}, Headers),
    ExpectedResponse = result_no_answers(Question),
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
            ExpectedResponse = result_no_answers(Question),
            ExpectedResponse = Response,
            Acc
        end,
    ok = lists:foldr(F, ok, TestCases).
