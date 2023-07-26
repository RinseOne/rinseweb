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
    Answer = rinseweb_test:request_and_decode_answer("uuid"),
    <<"text">> = maps:get(<<"type">>, Answer),
    true = uuid:is_v4(uuid:string_to_uuid(maps:get(<<"text">>, maps:get(<<"answer">>, Answer)))),
    ok.
