-module(wikipedia_web_SUITE).

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
    Answer = rinseweb_test:request_and_decode_answer("wiki hello"),
    <<"wiki">> = maps:get(<<"type">>, Answer),
    AnswerCustom = maps:get(<<"answer">>, Answer),
    true = is_list(AnswerCustom),
    true = length(AnswerCustom) > 0,
    [FirstItem|_] = AnswerCustom,
    true = maps:is_key(<<"title">>, FirstItem),
    true = maps:is_key(<<"snippet">>, FirstItem),
    true = maps:is_key(<<"url">>, FirstItem),
    ok.
