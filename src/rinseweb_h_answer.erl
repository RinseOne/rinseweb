%%%-------------------------------------------------------------------
%% @doc rinseweb answer handler
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_h_answer).

%% API
-export([init/2]).
-export([content_types_provided/2]).
-export([answer_to_html/2]).
-export([answer_to_json/2]).
-export([answer_to_text/2]).

%%====================================================================
%% API
%%====================================================================

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {[
        {<<"text/html">>, answer_to_html},
        {<<"application/json">>, answer_to_json},
        {<<"text/plain">>, answer_to_text}
    ], Req, State}.

answer_to_html(Req, State) ->
    Question = cowboy_req:binding(question, Req, <<>>),
    Answers = rinseweb_wiz:answer(Question),
    Answer = answers_to_binary(Answers),
    QuestionSafe = htmlentities(Question),
    AnswerSafe = htmlentities(Answer),
    Body = <<"<html>
<head>
    <meta charset=\"utf-8\">
    <title>rinse answer</title>
</head>
<body>
    <center>
        <p>", QuestionSafe/binary, "</p>
        <p>", AnswerSafe/binary, "</p>
    </center>
</body>
</html>">>,
    {Body, Req, State}.

answer_to_json(Req, State) ->
    Question = cowboy_req:binding(question, Req, <<>>),
    Answer = rinseweb_wiz:answer(Question),
    AnswerBin = jsx:encode(Answer),
    {AnswerBin, Req, State}.

answer_to_text(Req, State) ->
    Question = cowboy_req:binding(question, Req, <<>>),
    [#{short := Answer}] = rinseweb_wiz:answer(Question),
    {Answer, Req, State}.

%%====================================================================
%% Internal functions
%%====================================================================

answers_to_binary([shrug]) -> unicode:characters_to_binary("¯\\_(ツ)_/¯");
answers_to_binary([#{short := Answer}]) -> Answer.

htmlentities(Bin) when is_binary(Bin) ->
    ReplaceFuns = [
        fun(X) when is_binary(X) -> binary:replace(X, <<$<>>, <<"&lt;">>, [global]) end,
        fun(X) when is_binary(X) -> binary:replace(X, <<$>>>, <<"&gt;">>, [global]) end,
        fun(X) when is_binary(X) -> binary:replace(X, <<$&>>, <<"&amp;">>, [global]) end,
        fun(X) when is_binary(X) -> binary:replace(X, <<$">>, <<"&quot;">>, [global]) end,
        fun(X) when is_binary(X) -> binary:replace(X, <<$'>>, <<"&apos;">>, [global]) end
    ],
    F = fun(ReplaceFun, AccIn) -> ReplaceFun(AccIn) end,
    lists:foldr(F, Bin, ReplaceFuns).
