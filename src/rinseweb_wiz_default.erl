%%%-------------------------------------------------------------------
%% @doc rinseweb default wizard that knows nothing
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_wiz_default).

%% API
-export([answer/3]).

%%====================================================================
%% API
%%====================================================================

-spec answer(rinseweb_wiz:question(), [any()], rinseweb_req:req()) -> rinseweb_answer:answer().
answer(_Question, _Args, _Req) ->
    rinseweb_answer:new_shrug(default, <<"Unrecognized command">>).
