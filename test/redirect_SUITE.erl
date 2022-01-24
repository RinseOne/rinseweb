-module(redirect_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([ddg/1]).
-export([ddgi/1]).
-export([ddgv/1]).
-export([ddgn/1]).
-export([ddgm/1]).

%% ============================================================================
%% ct functions
%% ============================================================================

all() ->
    [
        ddg,
        ddgi,
        ddgv,
        ddgn,
        ddgm
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

result(Query, Source, Url) ->
    #{
        type => redirect,
        source => redirect,
        answer => #{
            url => Url,
            source => Source,
            query => Query
        }
    }.

%% ============================================================================
%% Tests
%% ============================================================================

ddg(_) ->
    Command = <<"ddg">>,
    Query = <<"hello">>,
    Question = <<Command/binary, " ", Query/binary>>,
    ExpectedAnswer = result(Query, <<"DuckDuckGo">>, <<"https://duckduckgo.com/?t=rinseone&q=", Query/binary>>),
    Answer = rinseweb_wiz_redirect:answer(Question, [Command, Query]),
    ExpectedAnswer = Answer,
    ok.

ddgi(_) ->
    Command = <<"ddgi">>,
    Query = <<"hello">>,
    Question = <<Command/binary, " ", Query/binary>>,
    ExpectedAnswer = result(Query, <<"DuckDuckGo images">>, <<"https://duckduckgo.com/?t=rinseone&iax=images&ia=images&q=", Query/binary>>),
    Answer = rinseweb_wiz_redirect:answer(Question, [Command, Query]),
    ExpectedAnswer = Answer,
    ok.

ddgv(_) ->
    Command = <<"ddgv">>,
    Query = <<"hello">>,
    Question = <<Command/binary, " ", Query/binary>>,
    ExpectedAnswer = result(Query, <<"DuckDuckGo videos">>, <<"https://duckduckgo.com/?t=rinseone&iax=videos&ia=videos&q=", Query/binary>>),
    Answer = rinseweb_wiz_redirect:answer(Question, [Command, Query]),
    ExpectedAnswer = Answer,
    ok.

ddgn(_) ->
    Command = <<"ddgn">>,
    Query = <<"hello">>,
    Question = <<Command/binary, " ", Query/binary>>,
    ExpectedAnswer = result(Query, <<"DuckDuckGo news">>, <<"https://duckduckgo.com/?t=rinseone&iar=news&ia=news&q=", Query/binary>>),
    Answer = rinseweb_wiz_redirect:answer(Question, [Command, Query]),
    ExpectedAnswer = Answer,
    ok.

ddgm(_) ->
    Command = <<"ddgm">>,
    Query = <<"hello">>,
    Question = <<Command/binary, " ", Query/binary>>,
    ExpectedAnswer = result(Query, <<"DuckDuckGo maps">>, <<"https://duckduckgo.com/?t=rinseone&ia=news&iaxm=places&q=", Query/binary>>),
    Answer = rinseweb_wiz_redirect:answer(Question, [Command, Query]),
    ExpectedAnswer = Answer,
    ok.
