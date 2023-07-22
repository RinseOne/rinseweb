%%%-------------------------------------------------------------------
%% @doc rinseweb Wikipedia wizard
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_wiz_wikipedia).

%% API
-export([answer/3]).

-define(URL, "https://en.wikipedia.org/w/api.php?action=query&format=json&generator=search&gsrprop=snippet&prop=info&inprop=url&gsrsearch=").
-define(SOURCE, wiki).

%%====================================================================
%% API
%%====================================================================

-spec answer(rinseweb_wiz:question(), [any()], rinseweb_req:req()) -> rinseweb_answer:answer().
answer(_Question, [Query], _) ->
    search(binary_to_list(Query)).

%%====================================================================
%% Internal functions
%%====================================================================

-spec search(string()) -> rinseweb_answer:answer().
search(Query) ->
    Items = rinseweb_be_wiki:search(?URL, Query),
    rinseweb_be_wiki:items_to_answer(?SOURCE, Items).
