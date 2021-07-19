-module(hash_SUITE).

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
    Config.

end_per_suite(_) ->
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%% ============================================================================
%% Helpers
%% ============================================================================

result(Hash) ->
    #{
        type => hash,
        source => hash,
        answer => #{
            hash => Hash
        }
    }.

%% ============================================================================
%% Tests
%% ============================================================================

sha(_) ->
    Question = <<"sha hello">>,
    ExpectedAnswer = result(<<"aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d">>),
    Answer = rinseweb_wiz_hash:answer(Question, [<<"sha">>, <<"hello">>]),
    ExpectedAnswer = Answer,
    ok.

sha2(_) ->
    Question = <<"sha2 hello">>,
    ExpectedAnswer = result(<<"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824">>),
    Answer = rinseweb_wiz_hash:answer(Question, [<<"sha2">>, <<"hello">>]),
    ExpectedAnswer = Answer,
    ok.

md5(_) ->
    Question = <<"md5 hello">>,
    ExpectedAnswer = result(<<"5d41402abc4b2a76b9719d911017c592">>),
    Answer = rinseweb_wiz_hash:answer(Question, [<<"md5">>, <<"hello">>]),
    ExpectedAnswer = Answer,
    ok.
