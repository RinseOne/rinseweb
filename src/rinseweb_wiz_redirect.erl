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
-define(URL_DDG_WEB, <<"https://duckduckgo.com/?t=rinseone&q=">>).
-define(URL_DDG_IMG, <<"https://duckduckgo.com/?t=rinseone&iax=images&ia=images&q=">>).
-define(URL_DDG_VID, <<"https://duckduckgo.com/?t=rinseone&iax=videos&ia=videos&q=">>).
-define(URL_DDG_NEW, <<"https://duckduckgo.com/?t=rinseone&iar=news&ia=news&q=">>).
-define(URL_DDG_MAP, <<"https://duckduckgo.com/?t=rinseone&ia=news&iaxm=places&q=">>).

%%====================================================================
%% API
%%====================================================================

-spec answer(rinseweb_wiz:question(), [any()]) -> rinseweb_wiz:answer().
answer(_Question, [Command, Query]) ->
    QueryEncoded = rinseweb_util:url_encode(Query),
    BaseUrl = command_to_base_url(Command),
    Url = <<BaseUrl/binary, QueryEncoded/binary>>,
    rinseweb_wiz:answer(?ANSWER_TYPE, ?ANSWER_SOURCE, create_answer(Url, Query)).

%%====================================================================
%% Internal functions
%%====================================================================

-spec command_to_base_url(binary()) -> binary().
command_to_base_url(<<"ddg">>) -> ?URL_DDG_WEB;
command_to_base_url(<<"ddgi">>) -> ?URL_DDG_IMG;
command_to_base_url(<<"ddgv">>) -> ?URL_DDG_VID;
command_to_base_url(<<"ddgn">>) -> ?URL_DDG_NEW;
command_to_base_url(<<"ddgm">>) -> ?URL_DDG_MAP.

-spec create_answer(binary(), binary()) -> answer().
create_answer(Url, Query) ->
    #{
        url => Url,
        query => Query
    }.
