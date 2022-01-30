-module(apply_web_SUITE).

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

%% ============================================================================
%% ct functions
%% ============================================================================

all() ->
    [
        add,
        subtract,
        multiply,
        divide
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

add(_) ->
    Question = "+ 1 2 3",
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, ResponseBody}} = rinseweb_test:request_json(Question),
    true = lists:member({"content-type","application/json"}, Headers),
    ResponseMap = rinseweb_test:decode_response_body(ResponseBody),
    ExpectedQuestion = list_to_binary(Question),
    ExpectedQuestion = maps:get(<<"question">>, ResponseMap),
    [Answer|_] = maps:get(<<"answers">>, ResponseMap),
    <<"number">> = maps:get(<<"type">>, Answer),
    6 = maps:get(<<"number">>, maps:get(<<"answer">>, Answer)),
    ok.

subtract(_) ->
    Question = "- 1 2 3",
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, ResponseBody}} = rinseweb_test:request_json(Question),
    true = lists:member({"content-type","application/json"}, Headers),
    ResponseMap = rinseweb_test:decode_response_body(ResponseBody),
    ExpectedQuestion = list_to_binary(Question),
    ExpectedQuestion = maps:get(<<"question">>, ResponseMap),
    [Answer|_] = maps:get(<<"answers">>, ResponseMap),
    <<"number">> = maps:get(<<"type">>, Answer),
    -4 = maps:get(<<"number">>, maps:get(<<"answer">>, Answer)),
    ok.

multiply(_) ->
    Question = "* 1 2 3",
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, ResponseBody}} = rinseweb_test:request_json(Question),
    true = lists:member({"content-type","application/json"}, Headers),
    ResponseMap = rinseweb_test:decode_response_body(ResponseBody),
    ExpectedQuestion = list_to_binary(Question),
    ExpectedQuestion = maps:get(<<"question">>, ResponseMap),
    [Answer|_] = maps:get(<<"answers">>, ResponseMap),
    <<"number">> = maps:get(<<"type">>, Answer),
    6 = maps:get(<<"number">>, maps:get(<<"answer">>, Answer)),
    ok.

divide(_) ->
    Question = "/ 4 2 2 2",
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, ResponseBody}} = rinseweb_test:request_json(Question),
    true = lists:member({"content-type","application/json"}, Headers),
    ResponseMap = rinseweb_test:decode_response_body(ResponseBody),
    ExpectedQuestion = list_to_binary(Question),
    ExpectedQuestion = maps:get(<<"question">>, ResponseMap),
    [Answer|_] = maps:get(<<"answers">>, ResponseMap),
    <<"number">> = maps:get(<<"type">>, Answer),
    0.5 = maps:get(<<"number">>, maps:get(<<"answer">>, Answer)),
    ok.
