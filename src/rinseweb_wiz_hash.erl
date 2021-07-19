%%%-------------------------------------------------------------------
%% @doc rinseweb hash wizard
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_wiz_hash).

%% API
-export([answer/2]).

-define(ANSWER_SOURCE, hash).
-define(ANSWER_TYPE, hash).

%%====================================================================
%% API
%%====================================================================

-spec answer(rinseweb_wiz:question(), [any()]) -> rinseweb_wiz:answer().
answer(_Question, [<<"sha">>, Bin]) ->
    Hash = format(crypto:hash(sha, Bin)),
    answer(Hash);
answer(_Question, [<<"sha2">>, Bin]) ->
    Hash = format(crypto:hash(sha256, Bin)),
    answer(Hash);
answer(_Question, [<<"md5">>, Bin]) ->
    Hash = format(crypto:hash(md5, Bin)),
    answer(Hash).


%%====================================================================
%% Internal functions
%%====================================================================

-spec answer(binary()) -> rinseweb_wiz:answer().
answer(Hash) ->
    AnswerCustom = answer_custom(Hash),
    rinseweb_wiz:answer(?ANSWER_TYPE, ?ANSWER_SOURCE, AnswerCustom).

-spec format(binary()) -> binary().
format(Bin) ->
    Str = string:lowercase(lists:flatten([[io_lib:format("~2.16.0B",[X]) || <<X:8>> <= Bin ]])),
    list_to_binary(Str).

-spec answer_custom(binary()) -> map().
answer_custom(Hash) ->
    #{hash => Hash}.
