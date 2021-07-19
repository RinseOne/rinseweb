-module(uuid_web_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([uuid_simple/1]).

%% ============================================================================
%% ct functions
%% ============================================================================

all() ->
    [
        uuid_simple
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

uuid_simple(_) ->
    Question = "uuid",
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, ResponseBody}} = rinseweb_test:request_json(Question),
    true = lists:member({"content-type","application/json"}, Headers),
    ResponseMap = rinseweb_test:decode_response_body(ResponseBody),
    ExpectedQuestion = list_to_binary(Question),
    ExpectedQuestion = maps:get(<<"question">>, ResponseMap),
    [Answer|_] = maps:get(<<"answers">>, ResponseMap),
    <<"text">> = maps:get(<<"type">>, Answer),
    true = uuid:is_v4(uuid:string_to_uuid(maps:get(<<"text">>, maps:get(<<"answer">>, Answer)))),
    ok.
