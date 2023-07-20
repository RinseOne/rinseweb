%%%-------------------------------------------------------------------
%% @doc rinseweb UUID wizard
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_wiz_uuid).

%% API
-export([answer/2]).

%%====================================================================
%% API
%%====================================================================

-spec answer(rinseweb_wiz:question(), [any()]) -> rinseweb_answer:answer().
answer(_Question, []) ->
    Uuid = list_to_binary(uuid:uuid_to_string(uuid:get_v4())),
    rinseweb_answer:new_text(text, uuid, Uuid).
