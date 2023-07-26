-module(rinseweb_test).

%% API
-export([base_uri/0]).
-export([request_and_decode_answer/1]).
-export([request_and_decode_answer/2]).

%%====================================================================
%% API
%%====================================================================

base_uri() ->
    Port = integer_to_list(rinseweb_env:port()),
    "http://localhost:" ++ Port.

request_and_decode_answer(Question) ->
    request_and_decode_answer(Question, []).

request_and_decode_answer(Question, ExtraHeaders) ->
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, ResponseBody}} = request_json(Question, ExtraHeaders),
    true = lists:member({"content-type","application/json"}, Headers),
    ResponseMap = decode_response_body(ResponseBody),
    ExpectedQuestion = rinseweb_util:binary_trim(list_to_binary(Question)),
    ExpectedQuestion = maps:get(<<"question">>, ResponseMap),
    [Answer|_] = maps:get(<<"answers">>, ResponseMap),
    true = is_map(Answer),
    true = maps:is_key(<<"type">>, Answer),
    Answer.

%%====================================================================
%% Internal functions
%%====================================================================

question_uri(Question) ->
     base_uri() ++ "/answer/" ++ edoc_lib:escape_uri(Question).

request_json(Question, ExtraHeaders) ->
    Headers = [{"Accept", "application/json"}|ExtraHeaders],
    Request = {question_uri(Question), Headers},
    httpc:request(get, Request, [], []).

decode_response_body(ResponseBody) ->
    jsx:decode(list_to_binary(ResponseBody), [return_maps]).
