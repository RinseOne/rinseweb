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

-spec answer(rinseweb_wiz:question(), [any()]) -> rinseweb_wiz:answer().
answer(_Question, _Args) ->
    #{
        type => shrug,
        source => default,
        answer => <<"Unrecognized command">>
    }.
