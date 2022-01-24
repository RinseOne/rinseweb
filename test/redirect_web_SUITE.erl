-module(redirect_web_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([ddg/1]).

%% ============================================================================
%% ct functions
%% ============================================================================

all() ->
    [
        ddg
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

result(Question, Query, Source, Url) ->
    #{
        <<"question">> => list_to_binary(Question),
        <<"answers">> => [
            #{
                <<"source">> => <<"redirect">>,
                <<"type">> => <<"redirect">>,
                <<"answer">> => #{
                    <<"url">> => Url,
                    <<"source">> => Source,
                    <<"query">> => Query
                }
            }
        ]
    }.

%% ============================================================================
%% Tests
%% ============================================================================

ddg(_) ->
    Question = "ddg hello",
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, ResponseBody}} = rinseweb_test:request_json(Question),
    Response = rinseweb_test:decode_response_body(ResponseBody),
    true = lists:member({"content-type","application/json"}, Headers),
    ExpectedResponse = result(Question, <<"hello">>, <<"DuckDuckGo">>, <<"https://duckduckgo.com/?t=rinseone&q=hello">>),
    ExpectedResponse = Response,
    ok.

ddg_encode_query(_) ->
    Question = "ddg hello & goodbye",
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, ResponseBody}} = rinseweb_test:request_json(Question),
    Response = rinseweb_test:decode_response_body(ResponseBody),
    true = lists:member({"content-type","application/json"}, Headers),
    ExpectedResponse = result(Question, <<"hello & goodbye">>, <<"DuckDuckGo">>, <<"https://duckduckgo.com/?t=rinseone&q=hello%20%26%20goodbye">>),
    ExpectedResponse = Response,
    ok.
