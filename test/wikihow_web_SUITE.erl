-module(wikihow_web_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([search/1]).

%% ============================================================================
%% ct functions
%% ============================================================================

all() ->
    [
        search
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

search(_) ->
    Question = "how do I say hello",
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, ResponseBody}} = rinseweb_test:request_json(Question),
    true = lists:member({"content-type","application/json"}, Headers),
    ResponseMap = rinseweb_test:decode_response_body(ResponseBody),
    ExpectedQuestion = list_to_binary(Question),
    ExpectedQuestion = maps:get(<<"question">>, ResponseMap),
    [Answer|_] = maps:get(<<"answers">>, ResponseMap),
    <<"wiki">> = maps:get(<<"type">>, Answer),
    AnswerCustom = maps:get(<<"answer">>, Answer),
    true = is_list(AnswerCustom),
    true = length(AnswerCustom) > 0,
    [FirstItem|_] = AnswerCustom,
    true = maps:is_key(<<"title">>, FirstItem),
    true = maps:is_key(<<"snippet">>, FirstItem),
    true = maps:is_key(<<"url">>, FirstItem),
    ok.
