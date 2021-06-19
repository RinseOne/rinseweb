%%%-------------------------------------------------------------------
%% @doc rinseweb hash wizard
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_wiz_hash).

%% API
-export([answer/2]).

%%====================================================================
%% API
%%====================================================================

-spec answer(rinseweb_wiz:question(), [any()]) -> rinseweb_wiz:answer().
answer(Question, [<<"sha">>, Bin]) ->
    #{
        question => Question,
        type => text,
        short => format(crypto:hash(sha, Bin))
    };
answer(Question, [<<"sha2">>, Bin]) ->
    #{
        question => Question,
        type => text,
        short => format(crypto:hash(sha256, Bin))
    };
answer(Question, [<<"md5">>, Bin]) ->
    #{
        question => Question,
        type => text,
        short => format(crypto:hash(md5, Bin))
    }.


%%====================================================================
%% Internal functions
%%====================================================================

-spec format(binary()) -> binary().
format(Bin) ->
    Str = string:lowercase(lists:flatten([[io_lib:format("~2.16.0B",[X]) || <<X:8>> <= Bin ]])),
    list_to_binary(Str).
