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
    Result = rinseweb_wiz:answer(Question),
    AnswerBin = result_to_binary(Result),
    QuestionSafe = htmlentities(Question),
    AnswerSafe = htmlentities(AnswerBin),
    Body = <<"<html>
<head>
    <meta charset=\"utf-8\">
	<title>rinse</title>
    <style type=\"text/css\">
        h2 {font-size:5em;font-family:monospace;}
        #question {font-size:3em;font-family:sans-serif;}
        #answer {font-size:3em;font-family:sans-serif;}
        footer {position:fixed;bottom:0;text-align:center;width:100%;font-size:1em;font-family:sans-serif;}
    </style>
</head>
<body>
    <center>
        <h2><span style=\"color:#0f2557\">r</span><span style=\"color:#28559a\">i</span><span style=\"color:#3778c2\">n</span><span style=\"color:#4b9fe1\">s</span><span style=\"color:#63bce5\">e</span></h2>
        <p id=\"question\">", QuestionSafe/binary, "</p>
        <p id=\"answer\">", AnswerSafe/binary, "</p>
    </center>
<footer>
    <p><a href=\"/about\">About</a> - <a href=\"/commands\">Commands</a></p>
</footer>
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
    Result = rinseweb_wiz:answer(Question),
    AnswerBin = result_to_binary(Result),
    {AnswerBin, Req, State}.

%%====================================================================
%% Internal functions
%%====================================================================

result_to_binary(#{answers := []}) -> unicode:characters_to_binary("¯\\_(ツ)_/¯");
result_to_binary(#{answers := [#{type := Type, answer := AnswerCustom}|_]}) ->
    answer_to_binary(Type, AnswerCustom).

-spec answer_to_binary(atom(), map()) -> binary().
answer_to_binary(text, #{text := Text}) -> Text;
answer_to_binary(hash, #{hash := Hash}) -> Hash;
answer_to_binary(conversion_result, AnswerCustom) ->
    UnitFrom = maps:get(unit_from_name, AnswerCustom),
    UnitFromNum = maps:get(unit_from_number, AnswerCustom),
    UnitTo = maps:get(unit_to_name, AnswerCustom),
    UnitToNum = maps:get(unit_to_number, AnswerCustom),
    UnitFromNumBin = number_to_binary(UnitFromNum),
    UnitToNumBin = number_to_binary(UnitToNum),
    <<UnitFromNumBin/binary, " ", UnitFrom/binary, " = ", UnitToNumBin/binary, " ", UnitTo/binary>>.

-spec number_to_binary(number()) -> binary().
number_to_binary(Num) when is_integer(Num) -> integer_to_binary(Num);
number_to_binary(Num) when is_float(Num) -> float_to_binary(Num, [{decimals, 10}, compact]).

-spec htmlentities(binary()) -> binary().
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
