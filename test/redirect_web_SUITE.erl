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
-export([ddg_encode_query/1]).

%% ============================================================================
%% ct functions
%% ============================================================================

all() ->
    [
        ddg,
        ddg_encode_query
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

expected_answer(Query, Source, Url) ->
    #{
        <<"source">> => <<"redirect">>,
        <<"type">> => <<"redirect">>,
        <<"answer">> => #{
            <<"url">> => Url,
            <<"source">> => Source,
            <<"query">> => Query
        }
    }.

%% ============================================================================
%% Tests
%% ============================================================================

ddg(_) ->
    Answer = rinseweb_test:request_and_decode_answer("ddg hello"),
    ExpectedAnswer = expected_answer(<<"hello">>, <<"DuckDuckGo">>, <<"https://duckduckgo.com/?t=rinseone&q=hello">>),
    ExpectedAnswer = Answer,
    ok.

ddg_encode_query(_) ->
    Answer = rinseweb_test:request_and_decode_answer("ddg hello & goodbye"),
    ExpectedAnswer = expected_answer(<<"hello & goodbye">>, <<"DuckDuckGo">>, <<"https://duckduckgo.com/?t=rinseone&q=hello%20%26%20goodbye">>),
    ExpectedAnswer = Answer,
    ok.
