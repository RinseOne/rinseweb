%%%-------------------------------------------------------------------
%% @doc rinseweb redirect wizard
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_wiz_redirect).

%% API
-export([answer/2]).

%% Types
-type answer() :: #{
    url => binary(),
    query => binary()
}.

-define(ANSWER_SOURCE, redirect).
-define(ANSWER_TYPE, redirect).
-define(URL_DDG, <<"https://duckduckgo.com/?t=rinseone&q=">>).

%%====================================================================
%% API
%%====================================================================

-spec answer(rinseweb_wiz:question(), [any()]) -> rinseweb_wiz:answer().
answer(_Question, [<<"ddg">>, Query]) ->
    QueryEncoded = rinseweb_util:url_encode(Query),
    Url = <<?URL_DDG/binary, QueryEncoded/binary>>,
    rinseweb_wiz:answer(?ANSWER_TYPE, ?ANSWER_SOURCE, create_answer(Url, Query)).

%%====================================================================
%% Internal functions
%%====================================================================

-spec create_answer(binary(), binary()) -> answer().
create_answer(Url, Query) ->
    #{
        url => Url,
        query => Query
    }.
