-module(rinseweb_test).

%% API
-export([question_uri/1]).
-export([request_json/1]).
-export([decode_response_body/1]).

%%====================================================================
%% API
%%====================================================================

question_uri(Question) ->
    "http://localhost:8080/answer/" ++ edoc_lib:escape_uri(Question).

request_json(Question) ->
    Request = {question_uri(Question), [{"Accept", "application/json"}]},
    httpc:request(get, Request, [], []).

decode_response_body(ResponseBody) ->
    jsx:decode(list_to_binary(ResponseBody), [return_maps]).
