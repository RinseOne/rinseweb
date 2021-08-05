%%%-------------------------------------------------------------------
%% @doc rinseweb wizard router
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_wiz).

%% API
-export([answer/1]).
-export([answer/3]).
-export([answer_text/3]).
-export([shrug/1]).

%% Types
-type result() :: #{
    question := binary(),
    answers := answers()
}.
-type question() :: binary().
-type answers() :: [answer()].
-type answer() :: #{
    type := answer_type(),
    source := answer_source(),
    answer => answer_custom()
}.
-type answer_type() :: conversion_result | definition | hash | shrug | text | wiki.
-type answer_source() :: atom().
-type answer_custom() :: any().
-type args() :: [any()].

-export_type([question/0]).
-export_type([result/0]).

%%====================================================================
%% API
%%====================================================================

-spec answer(question()) -> result().
answer(Question) ->
    TrimmedQuestion = binary_max_size(binary_trim(Question), 128),
    {Manifest, Args} = find_handler(TrimmedQuestion, rinseweb_manifests:get_all()),
    Answer = handler_answer(Manifest, TrimmedQuestion, Args),
    Answers = maybe_add_answer(Answer, []),
    #{
        question => TrimmedQuestion,
        answers => Answers
    }.

-spec answer(answer_type(), answer_source(), answer_custom()) -> answer().
answer(Type, Source, Custom) ->
    #{
        type => Type,
        source => Source,
        answer => Custom
    }.

-spec answer_text(answer_type(), answer_source(), binary()) -> answer().
answer_text(Type, Source, Bin) ->
    AnswerCustom = #{
        text => Bin
    },
    answer(Type, Source, AnswerCustom).

-spec shrug(answer_source()) -> answer().
shrug(Source) ->
    #{
        type => shrug,
        source => Source
    }.

%%====================================================================
%% Internal functions
%%====================================================================

-spec find_handler(question(), [rinseweb_manifests:manifest()]) -> {rinseweb_manifests:manifest(), args()} | nomatch.
find_handler(_Question, []) -> nomatch;
find_handler(Question, [#{matches := Matches} = Manifest|Rest]) ->
    Result = compare_manifest_matches(Question, Matches, Manifest),
    find_handler_check_result(Result, Question, Rest).

-type compare_result() :: {match, rinseweb_manifests:manifest(), args()} | nomatch.
-spec compare_manifest_matches(question(), [rinseweb_manifests:match()], rinseweb_manifests:manifest()) -> compare_result().
compare_manifest_matches(_Question, [], _Manifest) -> nomatch;
compare_manifest_matches(Question, [#{type := regex, value := Regex}|Rest], Manifest) ->
    case re:run(Question, Regex) of
        {match, [_First|RestRegexMatches]} ->
            CapturedArgs = regex_matches_to_bin(RestRegexMatches, Question, []),
            {match, Manifest, CapturedArgs};
        nomatch -> compare_manifest_matches(Question, Rest, Manifest)
    end.

-spec regex_matches_to_bin([{non_neg_integer(), non_neg_integer()}], binary(), [binary()]) -> [binary()].
regex_matches_to_bin([], _Bin, Acc) -> lists:reverse(Acc);
regex_matches_to_bin([{Pos, Len}|Rest], Bin, Acc) ->
    regex_matches_to_bin(Rest, Bin, [binary:part(Bin, Pos, Len)|Acc]).

-spec find_handler_check_result(compare_result(), question(), args()) -> {rinseweb_manifests:manifest(), args()}.
find_handler_check_result({match, Manifest, Args}, _Question, _Rest) ->
    {Manifest, Args};
find_handler_check_result(nomatch, Question, Rest) ->
    find_handler(Question, Rest).

-spec handler_answer(rinseweb_manifests:manifest(), question(), args()) -> answer().
handler_answer(#{handler := Handler, cache := _CacheOptions}, Question, Args) ->
    case rinseweb_cache:get(Handler, Question) of
        undefined ->
            Answer = Handler:answer(Question, Args),
            ok = rinseweb_cache:put(Handler, Question, Answer),
            Answer;
        Answer ->
            Answer
    end;
handler_answer(#{handler := Handler}, Question, Args) ->
    Handler:answer(Question, Args).

-spec maybe_add_answer(answer(), answers()) -> answers().
maybe_add_answer(#{type := shrug}, Answers) -> Answers;
maybe_add_answer(Answer, Answers) -> [Answer|Answers].

-spec binary_max_size(binary(), pos_integer()) -> binary().
binary_max_size(Bin, Size) when byte_size(Bin) =< Size -> Bin;
binary_max_size(Bin, Size) -> binary:part(Bin, 0, Size).

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
