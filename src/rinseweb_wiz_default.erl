%%%-------------------------------------------------------------------
%% @doc rinseweb default wizard that knows nothing
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_wiz_default).

%% API
-export([answer/2]).

%%====================================================================
%% API
%%====================================================================

-spec answer(rinseweb_wiz:question(), [any()]) -> rinseweb_answer:answer().
answer(_Question, _Args) ->
    rinseweb_answer:new_shrug(default, <<"Unrecognized command">>).
