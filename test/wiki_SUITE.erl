-module(wiki_SUITE).

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

search(_) ->
    Question = <<"wiki hello">>,
    Answer = rinseweb_wiz_wiki:answer(Question, [<<"hello">>]),
    wiki = maps:get(type, Answer),
    wiki = maps:get(source, Answer),
    AnswerCustom = maps:get(answer, Answer),
    true = is_list(AnswerCustom),
    true = length(AnswerCustom) > 0,
    [FirstItem|_] = AnswerCustom,
    true = maps:is_key(title, FirstItem),
    true = maps:is_key(snippet, FirstItem),
    true = maps:is_key(url, FirstItem).
