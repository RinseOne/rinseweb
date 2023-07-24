-module(wimip_web_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([simple/1]).
-export([x_forwarded_for_empty/1]).
-export([x_forwarded_for_ipv4/1]).
-export([x_forwarded_for_ipv6/1]).

%% ============================================================================
%% ct functions
%% ============================================================================

all() ->
    [
        simple,
        x_forwarded_for_empty,
        x_forwarded_for_ipv4,
        x_forwarded_for_ipv6
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
%% Tests
%% ============================================================================

simple(_) ->
    Question = "wimip",
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, ResponseBody}} = rinseweb_test:request_json(Question),
    true = lists:member({"content-type","application/json"}, Headers),
    ResponseMap = rinseweb_test:decode_response_body(ResponseBody),
    ExpectedQuestion = list_to_binary(Question),
    ExpectedQuestion = maps:get(<<"question">>, ResponseMap),
    [Answer|_] = maps:get(<<"answers">>, ResponseMap),
    <<"text">> = maps:get(<<"type">>, Answer),
    IpAddressBin = maps:get(<<"text">>, maps:get(<<"answer">>, Answer)),
    {ok, IpAddress} = inet:parse_address(binary_to_list(IpAddressBin)),
    % No X-Forwarded-For header should fall back on peer localhost IP
    true = inet:is_ip_address(IpAddress),
    ok.

x_forwarded_for_empty(_) ->
    Question = "wimip",
    ExtraHeaders = [{"X-Forwarded-For", ""}],
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, ResponseBody}} = rinseweb_test:request_json(Question, ExtraHeaders),
    true = lists:member({"content-type","application/json"}, Headers),
    ResponseMap = rinseweb_test:decode_response_body(ResponseBody),
    ExpectedQuestion = list_to_binary(Question),
    ExpectedQuestion = maps:get(<<"question">>, ResponseMap),
    [Answer|_] = maps:get(<<"answers">>, ResponseMap),
    <<"text">> = maps:get(<<"type">>, Answer),
    IpAddressBin = maps:get(<<"text">>, maps:get(<<"answer">>, Answer)),
    {ok, IpAddress} = inet:parse_address(binary_to_list(IpAddressBin)),
    % Empty X-Forwarded-For header should fall back on peer localhost IP
    true = inet:is_ip_address(IpAddress),
    ok.

x_forwarded_for_ipv4(_) ->
    Question = "wimip",
    IpAddressReqBin = <<"1.2.3.4">>,
    ExtraHeaders = [{"X-Forwarded-For", binary_to_list(IpAddressReqBin)}],
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, ResponseBody}} = rinseweb_test:request_json(Question, ExtraHeaders),
    true = lists:member({"content-type","application/json"}, Headers),
    ResponseMap = rinseweb_test:decode_response_body(ResponseBody),
    ExpectedQuestion = list_to_binary(Question),
    ExpectedQuestion = maps:get(<<"question">>, ResponseMap),
    [Answer|_] = maps:get(<<"answers">>, ResponseMap),
    <<"text">> = maps:get(<<"type">>, Answer),
    IpAddressBin = maps:get(<<"text">>, maps:get(<<"answer">>, Answer)),
    IpAddressReqBin = IpAddressBin,
    {ok, IpAddress} = inet:parse_address(binary_to_list(IpAddressBin)),
    true = inet:is_ip_address(IpAddress),
    ok.

x_forwarded_for_ipv6(_) ->
    Question = "wimip",
    IpAddressReqBin = <<"2001:db8::2:1">>,
    ExtraHeaders = [{"X-Forwarded-For", binary_to_list(IpAddressReqBin)}],
    {ok, {{"HTTP/1.1", 200, "OK"}, Headers, ResponseBody}} = rinseweb_test:request_json(Question, ExtraHeaders),
    true = lists:member({"content-type","application/json"}, Headers),
    ResponseMap = rinseweb_test:decode_response_body(ResponseBody),
    ExpectedQuestion = list_to_binary(Question),
    ExpectedQuestion = maps:get(<<"question">>, ResponseMap),
    [Answer|_] = maps:get(<<"answers">>, ResponseMap),
    <<"text">> = maps:get(<<"type">>, Answer),
    IpAddressBin = maps:get(<<"text">>, maps:get(<<"answer">>, Answer)),
    IpAddressReqBin = IpAddressBin,
    {ok, IpAddress} = inet:parse_address(binary_to_list(IpAddressBin)),
    true = inet:is_ip_address(IpAddress),
    ok.