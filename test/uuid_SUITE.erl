-module(uuid_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([simple/1]).

%% ============================================================================
%% ct functions
%% ============================================================================

all() ->
    [
        simple
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_) ->
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%% ============================================================================
%% Tests
%% ============================================================================

simple(_) ->
    Question = <<"uuid">>,
    Answer = rinseweb_wiz_uuid:answer(Question, []),
    Question = maps:get(question, Answer),
    text = maps:get(type, Answer),
    true = uuid:is_v4(uuid:string_to_uuid(maps:get(short, Answer))).
