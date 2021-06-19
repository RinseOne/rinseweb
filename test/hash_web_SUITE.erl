-module(hash_web_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([sha/1]).
-export([sha2/1]).
-export([md5/1]).

%% ============================================================================
%% ct functions
%% ============================================================================

all() ->
    [
        sha,
        sha2,
        md5
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

sha(_) ->
    Question = "sha hello",
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, ResponseBody}} = rinseweb_test:request_json(Question),
    Response = rinseweb_test:decode_response_body(ResponseBody),
    true = lists:member({"content-type","application/json"}, Headers),
    ExpectedResponse = [
        #{
            <<"question">> => <<"sha hello">>,
            <<"short">> => <<"aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d">>,
            <<"type">> => <<"text">>
        }
    ],
    ExpectedResponse = Response,
    ok.

sha2(_) ->
    Question = "sha2 hello",
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, ResponseBody}} = rinseweb_test:request_json(Question),
    Response = rinseweb_test:decode_response_body(ResponseBody),
    true = lists:member({"content-type","application/json"}, Headers),
    ExpectedResponse = [
        #{
            <<"question">> => <<"sha2 hello">>,
            <<"short">> => <<"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824">>,
            <<"type">> => <<"text">>
        }
    ],
    ExpectedResponse = Response,
    ok.

md5(_) ->
    Question = "md5 hello",
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, ResponseBody}} = rinseweb_test:request_json(Question),
    Response = rinseweb_test:decode_response_body(ResponseBody),
    true = lists:member({"content-type","application/json"}, Headers),
    ExpectedResponse = [
        #{
            <<"question">> => <<"md5 hello">>,
            <<"short">> => <<"5d41402abc4b2a76b9719d911017c592">>,
            <<"type">> => <<"text">>
        }
    ],
    ExpectedResponse = Response,
    ok.
