%%%-------------------------------------------------------------------
%% @doc rinseweb wizard router
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_wiz).

%% API
-export([answer/1]).

%% Types
-type answers() :: [answer()].
-type answer() :: shrug | answer_payload().
-type answer_type() :: text.
-type answer_payload() :: #{
    question := binary(),
    type := answer_type(),
    short := binary()
}.
-type question() :: binary().

-export_type([question/0]).
-export_type([answer/0]).

%%====================================================================
%% API
%%====================================================================

-spec answer(question()) -> answers().
answer(Question) ->
    TrimmedQuestion = binary_trim(Question),
    {Handler, Args} = find_handler(TrimmedQuestion, rinseweb_manifests:get_all()),
    [Handler:answer(TrimmedQuestion, Args)].

%%====================================================================
%% Internal functions
%%====================================================================

-spec find_handler(question(), [rinseweb_manifests:manifest()]) -> {atom(), [any()]} | nomatch.
find_handler(_Question, []) -> nomatch;
find_handler(Question, [#{matches := Matches, handler := Handler}|Rest]) ->
    Result = compare_manifest_matches(Question, Matches, Handler),
    find_handler_check_result(Result, Question, Rest).

-type compare_result() :: {match, atom(), [any()]} | nomatch.
-spec compare_manifest_matches(question(), [rinseweb_manifests:match()], atom()) -> compare_result().
compare_manifest_matches(_Question, [], _Handler) -> nomatch;
compare_manifest_matches(Question, [#{type := regex, value := Regex}|Rest], Handler) ->
    case re:run(Question, Regex) of
        {match, [_First|RestRegexMatches]} ->
            CapturedArgs = regex_matches_to_bin(RestRegexMatches, Question, []),
            {match, Handler, CapturedArgs};
        nomatch -> compare_manifest_matches(Question, Rest, Handler)
    end.

-spec regex_matches_to_bin([{non_neg_integer(), non_neg_integer()}], binary(), [binary()]) -> [binary()].
regex_matches_to_bin([], _Bin, Acc) -> lists:reverse(Acc);
regex_matches_to_bin([{Pos, Len}|Rest], Bin, Acc) ->
    regex_matches_to_bin(Rest, Bin, [binary:part(Bin, Pos, Len)|Acc]).

find_handler_check_result({match, Handler, Args}, _Question, _Rest) ->
    {Handler, Args};
find_handler_check_result(nomatch, Question, Rest) ->
    find_handler(Question, Rest).

-spec binary_trim(binary()) -> binary().
binary_trim(Bin) ->
    binary_ltrim(binary_rtrim(Bin)).

-spec binary_ltrim(binary()) -> binary().
binary_ltrim(<<32, Bin/binary>>) -> binary_ltrim(Bin);
binary_ltrim(Bin) -> Bin.

-spec binary_rtrim(binary()) -> binary().
binary_rtrim(Bin) when binary_part(Bin, {byte_size(Bin), -1}) =:= <<32>> ->
    binary_rtrim(binary_part(Bin, {0, byte_size(Bin) - 1}));
binary_rtrim(Bin) -> Bin.
