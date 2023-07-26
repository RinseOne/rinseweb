%%%-------------------------------------------------------------------
%% @doc rinseweb what is my wizard
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_wiz_whatismy).

%% API
-export([answer/3]).

-define(ANSWER_SOURCE, wimip).
-define(ANSWER_TYPE, text).

%%====================================================================
%% API
%%====================================================================

-spec answer(rinseweb_wiz:question(), [any()], rinseweb_req:req()) -> rinseweb_answer:answer().
answer(<<"wimip">>, [], #{client_ip := ClientIp}) ->
    client_ip_answer(ClientIp);
answer(_Question, [<<"what is my">>, <<"ip">>], #{client_ip := ClientIp}) ->
    client_ip_answer(ClientIp);
answer(<<"wimua">>, [], #{user_agent := UserAgent}) ->
    user_agent_answer(UserAgent);
answer(_Question, [<<"what is my">>, <<"user agent">>], #{user_agent := UserAgent}) ->
    user_agent_answer(UserAgent).

%%====================================================================
%% Internal functions
%%====================================================================

-spec client_ip_answer(inet:ip_address()) -> rinseweb_answer:answer().
client_ip_answer(ClientIp) ->
    rinseweb_answer:new_text(?ANSWER_TYPE, ?ANSWER_SOURCE, list_to_binary(inet:ntoa(ClientIp))).

-spec user_agent_answer(binary()) -> rinseweb_answer:answer().
user_agent_answer(UserAgent) ->
    rinseweb_answer:new_text(?ANSWER_TYPE, ?ANSWER_SOURCE, UserAgent).
