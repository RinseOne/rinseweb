-module(convert_web_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([convert_json/1]).
-export([convert_unrecognized_syntax/1]).
-export([convert_unsupported_unit/1]).
-export([convert_invalid_number/1]).

%% ============================================================================
%% ct functions
%% ============================================================================

all() ->
    [
        convert_json,
        convert_unrecognized_syntax,
        convert_unsupported_unit,
        convert_invalid_number
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(rinseweb),
    Config.

end_per_suite(_) ->
    ok = application:stop(rinseweb).

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%% ============================================================================
%% Helpers
%% ============================================================================

uri(Question) ->
    "http://localhost:8080/answer/" ++ edoc_lib:escape_uri(Question).

request_json(Question) ->
    Request = {uri(Question), [{"Accept", "application/json"}]},
    httpc:request(get, Request, [], []).

decode_response_body(ResponseBody) ->
    jsx:decode(list_to_binary(ResponseBody), [return_maps]).

unregonized_command(Question) when is_list(Question) ->
    [
        #{
            <<"question">> => list_to_binary(Question),
            <<"short">> => <<"No clue">>,
            <<"type">> => <<"text">>
        }
    ].

%% ============================================================================
%% Tests
%% ============================================================================

convert_json(_) ->
    Question = "convert 20 km to miles",
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, ResponseBody}} = request_json(Question),
    Response = decode_response_body(ResponseBody),
    true = lists:member({"content-type","application/json"}, Headers),
    ExpectedResponse = [
        #{
            <<"question">> => <<"convert 20 km to miles">>,
            <<"short">> => <<"20 kilometers is equal to 12.427424 miles">>,
            <<"type">> => <<"text">>
        }
    ],
    ExpectedResponse = Response,
    ok.

convert_unrecognized_syntax(_) ->
    Question = "convert foobar",
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, ResponseBody}} = request_json(Question),
    Response = decode_response_body(ResponseBody),
    true = lists:member({"content-type","application/json"}, Headers),
    ExpectedResponse = unregonized_command(Question),
    io:format("Expected response: ~p~n", [ExpectedResponse]),
    ExpectedResponse = Response,
    ok.

convert_unsupported_unit(_) ->
    Question = "convert 5 smoots to meters",
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, ResponseBody}} = request_json(Question),
    Response = decode_response_body(ResponseBody),
    true = lists:member({"content-type","application/json"}, Headers),
    [<<"shrug">>] = Response,
    ok.

convert_invalid_number(_) ->
    TestCases = [
        "", ".", ".1.", "..1", "1..",
        "-", "-.", "-.1.", "-..1", "-1.."
    ],
    F = fun(Num, Acc) ->
            Question = "convert " ++ Num ++ " kg to pounds",
            ExpectedResponse = unregonized_command(Question),
            {ok, {{"HTTP/1.1", 200, "OK"}, Headers, ResponseBody}} = request_json(Question),
            Response = decode_response_body(ResponseBody),
            true = lists:member({"content-type","application/json"}, Headers),
            ExpectedResponse = Response,
            Acc
        end,
    ok = lists:foldr(F, ok, TestCases).
