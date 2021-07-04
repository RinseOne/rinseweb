-module(timestamp_web_SUITE).

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
    {ok, _} = application:ensure_all_started(rinseweb),
    Config.

end_per_suite(_) ->
    ok = application:stop(rinseweb).

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%% ============================================================================
%% Tests
%% ============================================================================

seconds(_) ->
    Question = "1624433430",
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, ResponseBody}} = rinseweb_test:request_json(Question),
    Response = rinseweb_test:decode_response_body(ResponseBody),
    true = lists:member({"content-type","application/json"}, Headers),
    ExpectedResponse = [
        #{
            <<"question">> => <<"1624433430">>,
            <<"short">> => <<"2021-06-23 07:30:30 UTC">>,
            <<"type">> => <<"text">>
        }
    ],
    ExpectedResponse = Response,
    ok.

milliseconds(_) ->
    Question = "1624433430000",
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, ResponseBody}} = rinseweb_test:request_json(Question),
    Response = rinseweb_test:decode_response_body(ResponseBody),
    true = lists:member({"content-type","application/json"}, Headers),
    ExpectedResponse = [
        #{
            <<"question">> => <<"1624433430000">>,
            <<"short">> => <<"2021-06-23 07:30:30 UTC">>,
            <<"type">> => <<"text">>
        }
    ],
    ExpectedResponse = Response,
    ok.

timestamp(_) ->
    TestStartTime = erlang:system_time(second),
    Questions = ["now", "timestamp", "unix timestamp"],
    F = fun(Question, Acc) ->
            {ok, {{"HTTP/1.1", 200, "OK"}, Headers, ResponseBody}} = rinseweb_test:request_json(Question),
            [Answer|_] = rinseweb_test:decode_response_body(ResponseBody),
            true = lists:member({"content-type","application/json"}, Headers),
            <<"text">> = maps:get(<<"type">>, Answer),
            ExpectedQuestion = list_to_binary(Question),
            ExpectedQuestion = maps:get(<<"question">>, Answer),
            AnswerTimeBin = maps:get(<<"short">>, Answer),
            AnswerTimeInt = binary_to_integer(AnswerTimeBin),
            OneMinFromTestStart = TestStartTime + 60,
            true = AnswerTimeInt >= TestStartTime,
            true = AnswerTimeInt < OneMinFromTestStart,
            Acc
    end,
    ok = lists:foldr(F, ok, Questions).
