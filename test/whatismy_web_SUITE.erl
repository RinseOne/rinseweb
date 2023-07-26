-module(whatismy_web_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([ip_short/1]).
-export([ip_long/1]).
-export([ip_x_forwarded_for_empty/1]).
-export([ip_x_forwarded_for_ipv4/1]).
-export([ip_x_forwarded_for_ipv6/1]).
-export([ua_none/1]).
-export([ua_short/1]).
-export([ua_long/1]).

%% ============================================================================
%% ct functions
%% ============================================================================

all() ->
    [
        ip_short,
        ip_long,
        ip_x_forwarded_for_empty,
        ip_x_forwarded_for_ipv4,
        ip_x_forwarded_for_ipv6,
        ua_none,
        ua_short,
        ua_long
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

ip_short(_) ->
    Answer = rinseweb_test:request_and_decode_answer("wimip"),
    <<"text">> = maps:get(<<"type">>, Answer),
    IpAddressBin = maps:get(<<"text">>, maps:get(<<"answer">>, Answer)),
    {ok, IpAddress} = inet:parse_address(binary_to_list(IpAddressBin)),
    % No X-Forwarded-For header should fall back on peer localhost IP
    true = inet:is_ip_address(IpAddress),
    ok.

ip_long(_) ->
    Answer = rinseweb_test:request_and_decode_answer("what is my ip"),
    <<"text">> = maps:get(<<"type">>, Answer),
    IpAddressBin = maps:get(<<"text">>, maps:get(<<"answer">>, Answer)),
    {ok, IpAddress} = inet:parse_address(binary_to_list(IpAddressBin)),
    % No X-Forwarded-For header should fall back on peer localhost IP
    true = inet:is_ip_address(IpAddress),
    ok.

ip_x_forwarded_for_empty(_) ->
    Answer = rinseweb_test:request_and_decode_answer("wimip", [{"X-Forwarded-For", ""}]),
    <<"text">> = maps:get(<<"type">>, Answer),
    IpAddressBin = maps:get(<<"text">>, maps:get(<<"answer">>, Answer)),
    {ok, IpAddress} = inet:parse_address(binary_to_list(IpAddressBin)),
    % Empty X-Forwarded-For header should fall back on peer localhost IP
    true = inet:is_ip_address(IpAddress),
    ok.

ip_x_forwarded_for_ipv4(_) ->
    IpAddressReqBin = <<"1.2.3.4">>,
    ExtraHeaders = [{"X-Forwarded-For", binary_to_list(IpAddressReqBin)}],
    Answer = rinseweb_test:request_and_decode_answer("wimip", ExtraHeaders),
    <<"text">> = maps:get(<<"type">>, Answer),
    IpAddressBin = maps:get(<<"text">>, maps:get(<<"answer">>, Answer)),
    IpAddressReqBin = IpAddressBin,
    {ok, IpAddress} = inet:parse_address(binary_to_list(IpAddressBin)),
    true = inet:is_ip_address(IpAddress),
    ok.

ip_x_forwarded_for_ipv6(_) ->
    IpAddressReqBin = <<"2001:db8::2:1">>,
    ExtraHeaders = [{"X-Forwarded-For", binary_to_list(IpAddressReqBin)}],
    Answer = rinseweb_test:request_and_decode_answer("wimip", ExtraHeaders),
    <<"text">> = maps:get(<<"type">>, Answer),
    IpAddressBin = maps:get(<<"text">>, maps:get(<<"answer">>, Answer)),
    IpAddressReqBin = IpAddressBin,
    {ok, IpAddress} = inet:parse_address(binary_to_list(IpAddressBin)),
    true = inet:is_ip_address(IpAddress),
    ok.

ua_none(_) ->
    Answer = rinseweb_test:request_and_decode_answer("wimua"),
    <<"text">> = maps:get(<<"type">>, Answer),
    UserAgent = maps:get(<<"text">>, maps:get(<<"answer">>, Answer)),
    UserAgent = <<>>,
    ok.

ua_short(_) ->
    UserAgentHeaderValue = <<"Foo Bar">>,
    ExtraHeaders = [{"User-Agent", binary_to_list(UserAgentHeaderValue)}],
    Answer = rinseweb_test:request_and_decode_answer("wimua", ExtraHeaders),
    <<"text">> = maps:get(<<"type">>, Answer),
    UserAgent = maps:get(<<"text">>, maps:get(<<"answer">>, Answer)),
    UserAgentHeaderValue = UserAgent,
    ok.

ua_long(_) ->
    UserAgentHeaderValue = <<"Foo Bar">>,
    ExtraHeaders = [{"User-Agent", binary_to_list(UserAgentHeaderValue)}],
    Answer = rinseweb_test:request_and_decode_answer("what is my user agent", ExtraHeaders),
    <<"text">> = maps:get(<<"type">>, Answer),
    UserAgent = maps:get(<<"text">>, maps:get(<<"answer">>, Answer)),
    UserAgentHeaderValue = UserAgent,
    ok.
