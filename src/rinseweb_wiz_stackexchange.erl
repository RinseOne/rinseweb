%%%-------------------------------------------------------------------
%% @doc rinseweb stackexchange wizard
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_wiz_stackexchange).

%% Types
-type site() :: stackoverflow.

-type item() :: #{
    title := binary(),
    link := binary(),
    user := user()
}.

-type user() :: #{
    display_name := binary(),
    profile_image := binary()
}.

%% API
-export([answer/2]).

-define(URL, "https://api.stackexchange.com/2.3/search/advanced?order=desc&sort=activity&site=stackoverflow&q=").
-define(SOURCE, stackexchange).
-define(TYPE, stackexchange).

%%====================================================================
%% API
%%====================================================================

-spec answer(rinseweb_wiz:question(), [any()]) -> rinseweb_wiz:answer().
answer(_Question, [<<"so">>, Query]) ->
    search(stackoverflow, binary_to_list(Query));
answer(_Question, [<<"stackoverflow">>, Query]) ->
    search(stackoverflow, binary_to_list(Query)).

%%====================================================================
%% Internal functions
%%====================================================================

-spec search(site(), binary()) -> rinseweb_wiz:answer().
search(stackoverflow, Query) ->
    QueryEncoded = rinseweb_util:url_encode(Query),
    Url = ?URL ++ QueryEncoded,
    Req = {Url, []},
    Response = httpc:request(get, Req, [], [{body_format, binary}]),
    parse_response(Response).

-spec parse_response({ok, {httpc:status_line(), httpc:headers(), binary()}} | {error, term()}) -> rinseweb_wiz:answer().
parse_response({error, _Reason}) -> rinseweb_wiz:shrug(?SOURCE);
parse_response({ok, {{_Version, 200, _ReasonPhrase}, Headers, Body}}) ->
    BodyUnzipped = zlib:gunzip(Body),
    %io:format("XXX got headers: ~p~n", [Headers]),
    %io:format("XXX got response: ~p~n", [BodyUnzipped]),
    BodyDecoded = jsx:decode(BodyUnzipped, [{return_maps, true}]),
    rinseweb_wiz:answer(?TYPE, ?SOURCE, parse_body(BodyDecoded));
parse_response({ok, {{_Version, _Status, _ReasonPhrase}, _Headers, _Body}}) ->
    rinseweb_wiz:shrug(?SOURCE).

-spec parse_body(map()) -> [item()].
parse_body(#{<<"item">> := ListOfItems}) -> parse_body_items(ListOfItems, []);
parse_body(_) -> [].

-spec parse_body_items(list(), [item()]) -> [item()].
parse_body_items([Item|Rest], Acc) ->
    ParsedItem = parse_body_item(Item),
    parse_body_items(Rest, [ParsedItem|Acc]).

-spec parse_body_item(map()) -> item().
parse_body_item(Item) ->
    Title = maps:get(<<"title">>, Item, <<>>),
    Link = maps:get(<<"link">>, Item, <<>>),
    User = maps:get(<<"owner">>, Item, #{}),
    UserDisplayName = maps:get(<<"display_name">>, User, <<>>),
    UserProfileImage = maps:get(<<"profile_imagr">>, User, <<>>),
    #{
        title => Title,
        link => Link,
        user => #{
            display_name => UserDisplayName,
            profile_image => UserProfileImage
        }
    }.
