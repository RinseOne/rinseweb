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
    source => binary(),
    query => binary()
}.

-define(ANSWER_SOURCE, redirect).
-define(ANSWER_TYPE, redirect).
-define(URL_DDG_WEB, <<"https://duckduckgo.com/?t=rinseone&q=">>).
-define(URL_DDG_IMG, <<"https://duckduckgo.com/?t=rinseone&iax=images&ia=images&q=">>).
-define(URL_DDG_VID, <<"https://duckduckgo.com/?t=rinseone&iax=videos&ia=videos&q=">>).
-define(URL_DDG_NEW, <<"https://duckduckgo.com/?t=rinseone&iar=news&ia=news&q=">>).
-define(URL_DDG_MAP, <<"https://duckduckgo.com/?t=rinseone&ia=news&iaxm=places&q=">>).

%%====================================================================
%% API
%%====================================================================

-spec answer(rinseweb_wiz:question(), [any()]) -> rinseweb_answer:answer().
answer(_Question, [Command, Query]) ->
    QueryEncoded = rinseweb_util:url_encode(Query),
    {BaseUrl, Source} = command_to_url_and_source(Command),
    Url = <<BaseUrl/binary, QueryEncoded/binary>>,
    rinseweb_answer:new(?ANSWER_TYPE, ?ANSWER_SOURCE, create_answer(Url, Source, Query)).

%%====================================================================
%% Internal functions
%%====================================================================

-spec command_to_url_and_source(binary()) -> {binary(), binary()}.
command_to_url_and_source(<<"ddg">>) -> {?URL_DDG_WEB, <<"DuckDuckGo">>};
command_to_url_and_source(<<"ddgi">>) -> {?URL_DDG_IMG, <<"DuckDuckGo images">>};
command_to_url_and_source(<<"ddgv">>) -> {?URL_DDG_VID, <<"DuckDuckGo videos">>};
command_to_url_and_source(<<"ddgn">>) -> {?URL_DDG_NEW, <<"DuckDuckGo news">>};
command_to_url_and_source(<<"ddgm">>) -> {?URL_DDG_MAP, <<"DuckDuckGo maps">>}.

-spec create_answer(binary(), binary(), binary()) -> answer().
create_answer(Url, Source, Query) ->
    #{
        url => Url,
        source => Source,
        query => Query
    }.
