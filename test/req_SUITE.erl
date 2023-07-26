-module(req_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([new_from_cowboy_req_no_x_forwarded_for/1]).
-export([new_from_cowboy_req_invalid_x_forwarded_for/1]).
-export([new_from_cowboy_req_ipv4_x_forwarded_for/1]).
-export([new_from_cowboy_req_multiple_ipv4_x_forwarded_for/1]).
-export([new_from_cowboy_req_ipv6_x_forwarded_for/1]).
-export([new_from_cowboy_req_multiple_ipv6_x_forwarded_for/1]).
-export([new_from_cowboy_req_with_user_agent/1]).

%% ============================================================================
%% ct functions
%% ============================================================================

all() ->
    [
        new_from_cowboy_req_no_x_forwarded_for,
        new_from_cowboy_req_invalid_x_forwarded_for,
        new_from_cowboy_req_ipv4_x_forwarded_for,
        new_from_cowboy_req_multiple_ipv4_x_forwarded_for,
        new_from_cowboy_req_ipv6_x_forwarded_for,
        new_from_cowboy_req_multiple_ipv6_x_forwarded_for,
        new_from_cowboy_req_with_user_agent
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_) ->
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%% ============================================================================
%% Tests
%% ============================================================================

new_from_cowboy_req_no_x_forwarded_for(_) ->
    ClientIpTuple = {127, 0, 0, 1},
    Req = #{
        headers => #{},
        peer => {ClientIpTuple, 12345}
    },
    ActualReq = rinseweb_req:new_from_cowboy_req(Req),
    ExpectedReq = #{
        client_ip => ClientIpTuple,
        user_agent => <<>>
    },
    ExpectedReq = ActualReq.

new_from_cowboy_req_invalid_x_forwarded_for(_) ->
    ClientIpTuple = {127, 0, 0, 1},
    Req = #{
        headers => #{<<"x-forwarded-for">> => <<"foo">>},
        peer => {ClientIpTuple, 12345}
    },
    ActualReq = rinseweb_req:new_from_cowboy_req(Req),
    ExpectedReq = #{
        client_ip => ClientIpTuple,
        user_agent => <<>>
    },
    ExpectedReq = ActualReq.

new_from_cowboy_req_ipv4_x_forwarded_for(_) ->
    ClientIpTuple = {127, 0, 0, 1},
    Req = #{
        headers => #{<<"x-forwarded-for">> => <<"1.2.3.4">>},
        peer => {ClientIpTuple, 12345}
    },
    ActualReq = rinseweb_req:new_from_cowboy_req(Req),
    ExpectedReq = #{
        client_ip => {1, 2, 3, 4},
        user_agent => <<>>
    },
    ExpectedReq = ActualReq.

new_from_cowboy_req_multiple_ipv4_x_forwarded_for(_) ->
    ClientIpTuple = {127, 0, 0, 1},
    Req = #{
        headers => #{<<"x-forwarded-for">> => <<"1.1.1.1,2.2.2.2,3.3.3.3">>},
        peer => {ClientIpTuple, 12345}
    },
    ActualReq = rinseweb_req:new_from_cowboy_req(Req),
    ExpectedReq = #{
        client_ip => {1, 1, 1, 1},
        user_agent => <<>>
    },
    ExpectedReq = ActualReq.

new_from_cowboy_req_ipv6_x_forwarded_for(_) ->
    ClientIpTuple = {127, 0, 0, 1},
    Req = #{
        headers => #{<<"x-forwarded-for">> => <<"2001:db8::2:1">>},
        peer => {ClientIpTuple, 12345}
    },
    ActualReq = rinseweb_req:new_from_cowboy_req(Req),
    ExpectedReq = #{
        client_ip => {8193, 3512, 0, 0, 0, 0, 2, 1},
        user_agent => <<>>
    },
    ExpectedReq = ActualReq.

new_from_cowboy_req_multiple_ipv6_x_forwarded_for(_) ->
    ClientIpTuple = {127, 0, 0, 1},
    Req = #{
        headers => #{<<"x-forwarded-for">> => <<"2001:db8::2:1,2001:db8::2:2,2001:db8::2:3">>},
        peer => {ClientIpTuple, 12345}
    },
    ActualReq = rinseweb_req:new_from_cowboy_req(Req),
    ExpectedReq = #{
        client_ip => {8193, 3512, 0, 0, 0, 0, 2, 1},
        user_agent => <<>>
    },
    ExpectedReq = ActualReq.

new_from_cowboy_req_with_user_agent(_) ->
    UserAgent = <<"Foo Bar">>,
    ClientIpTuple = {127, 0, 0, 1},
    Req = #{
        headers => #{<<"user-agent">> => UserAgent},
        peer => {ClientIpTuple, 12345}
    },
    ActualReq = rinseweb_req:new_from_cowboy_req(Req),
    ExpectedReq = #{
        client_ip => ClientIpTuple,
        user_agent => UserAgent
    },
    ExpectedReq = ActualReq.
