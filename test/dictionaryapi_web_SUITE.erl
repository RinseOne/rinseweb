-module(dictionaryapi_web_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([query/1]).

%% ============================================================================
%% ct functions
%% ============================================================================

all() ->
    [
        query
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

query(_) ->
    Answer = rinseweb_test:request_and_decode_answer("define hello"),
    <<"definition">> = maps:get(<<"type">>, Answer),
    AnswerCustom = maps:get(<<"answer">>, Answer),
    true = is_list(AnswerCustom),
    true = length(AnswerCustom) > 0,
    [FirstItem|_] = AnswerCustom,
    true = maps:is_key(<<"word">>, FirstItem),
    true = maps:is_key(<<"phonetics">>, FirstItem),
    true = maps:is_key(<<"meanings">>, FirstItem),
    ok.
