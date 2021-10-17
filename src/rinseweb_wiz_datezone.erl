%%%-------------------------------------------------------------------
%% @doc rinseweb datezone wizard
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_wiz_datezone).

%% API
-export([answer/2]).

-define(ANSWER_SOURCE, datezone).
-define(ANSWER_TYPE, text).

%%====================================================================
%% API
%%====================================================================

-spec answer(rinseweb_wiz:question(), [any()]) -> rinseweb_wiz:answer().
answer(_Question, [Timezone]) ->
    Date = date_for_timezone(Timezone),
    rinseweb_wiz:answer_text(?ANSWER_TYPE, ?ANSWER_SOURCE, Date).

%%====================================================================
%% Internal functions
%%====================================================================

-spec date_for_timezone(binary()) -> binary().
date_for_timezone(Timezone) ->
    RawResult = os:cmd(["datezone", " '" ++ binary_to_list(Timezone) ++ "'", " 'now'"]),
    Result = string:trim(string:slice(RawResult, 0, string:str(RawResult, "\t"))),
    list_to_binary(Result).
