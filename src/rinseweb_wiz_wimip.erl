%%%-------------------------------------------------------------------
%% @doc rinseweb what is my ip wizard
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_wiz_wimip).

%% API
-export([answer/3]).

-define(ANSWER_SOURCE, wimip).
-define(ANSWER_TYPE, text).

%%====================================================================
%% API
%%====================================================================

-spec answer(rinseweb_wiz:question(), [any()], rinseweb_req:req()) -> rinseweb_answer:answer().
answer(_Question, [], #{client_ip := ClientIp}) ->
    rinseweb_answer:new_text(?ANSWER_TYPE, ?ANSWER_SOURCE, list_to_binary(inet:ntoa(ClientIp))).
