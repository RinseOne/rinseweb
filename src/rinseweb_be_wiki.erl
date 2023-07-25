%%%-------------------------------------------------------------------
%% @doc rinseweb backend for mediawiki API
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_be_wiki).

%% API
-export([search/2]).
-export([items_to_answer/2]).

%% Types
-type item() :: #{
    title := binary(),
    snippet := binary(),
    url := binary()
}.

-export_type([item/0]).

-define(TYPE, wiki).

%%====================================================================
%% API
%%====================================================================

-spec search(string(), string()) -> [item()].
search(BaseUri, Query) ->
    QueryEncoded = rinseweb_util:url_encode(Query),
    Url = BaseUri ++ QueryEncoded,
    Req = {Url, []},
    Response = httpc:request(get, Req, [], [{body_format, binary}]),
    {ok, Items} = parse_response(Response),
    Items.

-spec items_to_answer(rinseweb_answer:source(), [item()]) -> rinseweb_answer:answer().
items_to_answer(Source, []) -> rinseweb_answer:new_shrug(Source, <<"No results">>);
items_to_answer(Source, Items) -> rinseweb_answer:new(?TYPE, Source, Items).

%%====================================================================
%% Internal functions
%%====================================================================

-spec parse_response({ok, {rinseweb_util:httpc_status_line(), [rinseweb_util:httpc_http_header()], binary()}} | {error, term()}) ->
    {ok, [item()]} | {error, non_neg_integer(), string()}.
parse_response({error, Reason}) ->
    {error, 0, Reason};
parse_response({ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}}) ->
    BodyDecoded = jsx:decode(Body, [{return_maps, true}]),
    {ok, parse_body(BodyDecoded)};
parse_response({ok, {{_Version, Status, ReasonPhrase}, _Headers, _Body}}) ->
    {error, Status, ReasonPhrase}.

-spec parse_body(map()) -> [item()].
parse_body(#{<<"query">> := #{<<"pages">> := PagesMap}}) ->
    Pages = maps:values(PagesMap),
    lists:foldl(fun parse_page_map/2, [], Pages);
parse_body(_BodyDecoded) -> [].

-spec parse_page_map(map(), [item()]) -> [item()].
parse_page_map(#{<<"title">> := Title, <<"snippet">> := Snippet, <<"fullurl">> := Url}, Acc) ->
    Elem = #{title => Title, snippet => Snippet, url => Url},
    [Elem|Acc];
parse_page_map(#{<<"title">> := Title, <<"fullurl">> := Url}, Acc) ->
    Elem = #{title => Title, snippet => <<>>, url => Url},
    [Elem|Acc].
