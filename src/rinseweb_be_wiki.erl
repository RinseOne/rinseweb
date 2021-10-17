%%%-------------------------------------------------------------------
%% @doc rinseweb backend for mediawiki API
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_be_wiki).

%% API
-export([search/3]).

%% Types
-type item() :: #{
    title := binary(),
    snippet := binary(),
    url := binary()
}.

-define(TYPE, wiki).

%%====================================================================
%% API
%%====================================================================

-spec search(atom(), string(), string()) -> rinseweb_wiz:answer().
search(Source, BaseUri, Query) ->
    QueryEncoded = rinseweb_util:url_encode(Query),
    Url = BaseUri ++ QueryEncoded,
    Req = {Url, []},
    Response = httpc:request(get, Req, [], [{body_format, binary}]),
    parse_response(Source, Response).

%%====================================================================
%% Internal functions
%%====================================================================

-spec parse_response(atom(), {ok, {httpc:status_line(), httpc:headers(), binary()}} | {error, term()}) -> rinseweb_wiz:answer().
parse_response(Source, {error, _Reason}) -> rinseweb_wiz:shrug(Source);
parse_response(Source, {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}}) ->
    BodyDecoded = jsx:decode(Body, [{return_maps, true}]),
    rinseweb_wiz:answer(?TYPE, Source, parse_body(BodyDecoded));
parse_response(Source, {ok, {{_Version, _Status, _ReasonPhrase}, _Headers, _Body}}) ->
    rinseweb_wiz:shrug(Source).

-spec parse_body(map()) -> [item()].
parse_body(#{<<"query">> := #{<<"pages">> := PagesMap}}) ->
    Pages = maps:values(PagesMap),
    F = fun(#{<<"title">> := Title, <<"snippet">> := Snippet, <<"fullurl">> := Url}, Acc) ->
            Elem = #{title => Title, snippet => Snippet, url => Url},
            [Elem|Acc]
        end,
    lists:foldl(F, [], Pages);
parse_body(_BodyDecoded) -> [].
