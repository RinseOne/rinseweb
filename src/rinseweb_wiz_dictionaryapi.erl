%%%-------------------------------------------------------------------
%% @doc rinseweb dictionaryapi wizard
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_wiz_dictionaryapi).

%% API
-export([answer/2]).

%% Types
-type item() :: #{
    word := binary(),
    phonetics := [phonetics_item()],
    meanings := [meanings_item()]
}.

-type phonetics_item() :: #{
    text := binary(),
    audio => binary()
}.

-type meanings_item() :: #{
    part_of_speech := binary(),
    definitions := [definitions_item()]
}.

-type definitions_item() :: #{
    definition := binary(),
    example := binary()
}.

-define(URL, "https://api.dictionaryapi.dev/api/v2/entries/en_US/").
-define(SOURCE, definition).
-define(TYPE, definition).

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

-spec parse_body(list()) -> [item()].
parse_body(ListOfItems) ->
    lists:reverse(parse_body(ListOfItems, [])).

-spec parse_body(list(), [item()]) -> [item()].
parse_body([], Acc) -> Acc;
parse_body([Item|Rest], Acc) ->
    parse_body(Rest, [parse_item(Item)|Acc]).

-spec parse_item(map()) -> item().
parse_item(#{<<"word">> := Word, <<"phonetics">> := PhoneticsRaw, <<"meanings">> := MeaningsRaw}) ->
    FPhonetics =
        fun(#{<<"text">> := Text, <<"audio">> := Audio}, Acc) ->
                [#{text => Text, audio => Audio}|Acc];
            (#{<<"text">> := Text}, Acc) ->
                [#{text => Text}|Acc];
        (_, Acc) -> Acc
        end,
    Phonetics = lists:foldr(FPhonetics, [], PhoneticsRaw),
    FMeanings = fun(#{<<"partOfSpeech">> := PartOfSpeech, <<"definitions">> := DefinitionsRaw}, Acc) ->
            FDefinitions = fun(#{<<"definition">> := Definition, <<"example">> := Example}, AccDefs) ->
                    Elem = #{definition => Definition, example => Example},
                    [Elem|AccDefs];
                (#{<<"definition">> := Definition}, AccDefs) ->
                    Elem = #{definition => Definition},
                    [Elem|AccDefs]
            end,
            Definitions = lists:foldr(FDefinitions, [], DefinitionsRaw),
            Elem = #{part_of_speech => PartOfSpeech, definitions => Definitions},
            [Elem|Acc]
    end,
    Meanings = lists:foldr(FMeanings, [], MeaningsRaw),
    #{word => Word, phonetics => Phonetics, meanings => Meanings};
parse_item(_Item) -> [].
