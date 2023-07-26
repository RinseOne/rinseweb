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
%% Helpers
%% ============================================================================

expected_answer(Number) ->
    #{
        <<"source">> => <<"apply">>,
        <<"type">> => <<"number">>,
        <<"answer">> => #{
            <<"number">> => Number
        }
    }.

%% ============================================================================
%% Tests
%% ============================================================================

add(_) ->
    Answer = rinseweb_test:request_and_decode_answer("+ 1 2 3"),
    ExpectedAnswer = expected_answer(6),
    ExpectedAnswer = Answer,
    ok.

subtract(_) ->
    Answer = rinseweb_test:request_and_decode_answer("- 1 2 3"),
    ExpectedAnswer = expected_answer(-4),
    ExpectedAnswer = Answer,
    ok.

multiply(_) ->
    Answer = rinseweb_test:request_and_decode_answer("* 1 2 3"),
    ExpectedAnswer = expected_answer(6),
    ExpectedAnswer = Answer,
    ok.

divide(_) ->
    Answer = rinseweb_test:request_and_decode_answer("/ 4 2 2 2"),
    ExpectedAnswer = expected_answer(0.5),
    ExpectedAnswer = Answer,
    ok.
