%%%-------------------------------------------------------------------
%% @doc rinseweb Wikipedia wizard
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_wiz_wikipedia).

%% API
-export([answer/2]).

-define(URL, "https://en.wikipedia.org/w/api.php?action=query&format=json&generator=search&gsrprop=snippet&prop=info&inprop=url&gsrsearch=").
-define(SOURCE, wiki).

%%====================================================================
%% API
%%====================================================================

-spec answer(rinseweb_wiz:question(), [any()]) -> rinseweb_wiz:answer().
answer(_Question, [Query]) ->
    search(binary_to_list(Query)).

%%====================================================================
%% Internal functions
%%====================================================================

-spec search(string()) -> rinseweb_wiz:answer().
search(Query) ->
    Items = rinseweb_be_wiki:search(?URL, Query),
    rinseweb_be_wiki:items_to_answer(?SOURCE, Items).
