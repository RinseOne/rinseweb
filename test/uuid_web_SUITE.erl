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
%% Helpers
%% ============================================================================

uri(Question) ->
    "http://localhost:8080/answer/" ++ edoc_lib:escape_uri(Question).

request_json(Question) ->
    Request = {uri(Question), [{"Accept", "application/json"}]},
    httpc:request(get, Request, [], []).

decode_response_body(ResponseBody) ->
    jsx:decode(list_to_binary(ResponseBody), [return_maps]).

%% ============================================================================
%% Tests
%% ============================================================================

uuid_simple(_) ->
    Question = "uuid",
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, ResponseBody}} = request_json(Question),
    Response = decode_response_body(ResponseBody),
    true = lists:member({"content-type","application/json"}, Headers),
    [Response0|_] = Response,
    <<"uuid">> = maps:get(<<"question">>, Response0),
    <<"text">> = maps:get(<<"type">>, Response0),
    true = uuid:is_v4(uuid:string_to_uuid(maps:get(<<"short">>, Response0))),
    ok.
