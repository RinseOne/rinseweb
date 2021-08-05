%%%-------------------------------------------------------------------
%% @doc rinseweb wiki wizard
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_wiz_wiki).

%% API
-export([answer/2]).

%% Types
-type item() :: #{
    title := binary(),
    snippet := binary(),
    url := binary()
}.

-define(URL, "https://en.wikipedia.org/w/api.php?action=query&format=json&generator=search&gsrprop=snippet&prop=info&inprop=url&gsrsearch=").
-define(SOURCE, wiki).
-define(TYPE, wiki).

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
    QueryEncoded = rinseweb_util:url_encode(Query),
    Url = ?URL ++ QueryEncoded,
    Req = {Url, []},
    Response = httpc:request(get, Req, [], [{body_format, binary}]),
    parse_response(Response).

-spec parse_response({ok, {httpc:status_line(), httpc:headers(), binary()}} | {error, term()}) -> rinseweb_wiz:answer().
parse_response({error, _Reason}) -> rinseweb_wiz:shrug(?SOURCE);
parse_response({ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}}) ->
    BodyDecoded = jsx:decode(Body, [{return_maps, true}]),
    rinseweb_wiz:answer(?TYPE, ?SOURCE, parse_body(BodyDecoded));
parse_response({ok, {{_Version, _Status, _ReasonPhrase}, _Headers, _Body}}) ->
    rinseweb_wiz:shrug(?SOURCE).

-spec parse_body(map()) -> [item()].
parse_body(#{<<"query">> := #{<<"pages">> := PagesMap}}) ->
    Pages = maps:values(PagesMap),
    F = fun(#{<<"title">> := Title, <<"snippet">> := Snippet, <<"fullurl">> := Url}, Acc) ->
            Elem = #{title => Title, snippet => Snippet, url => Url},
            [Elem|Acc]
        end,
    lists:foldl(F, [], Pages);
parse_body(_BodyDecoded) -> [].
