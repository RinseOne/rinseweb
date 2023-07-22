%%%-------------------------------------------------------------------
%% @doc rinseweb request
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_req).

%% API
-export([new_from_cowboy_req/1]).

%% Types
-type req() :: #{
    client_ip := inet:ip_address()
}.

-spec new_from_cowboy_req(cowboy_req:req()) -> req().
new_from_cowboy_req(#{peer := {PeerIpAddress, _}} = Req) ->
    %% Per AWS docs https://docs.aws.amazon.com/elasticloadbalancing/latest/application/x-forwarded-headers.html
    %% by default load balancer will append (actually prepend) the client IP it sees to the
    %% X-Forwarded-For header. Here we'll check if that header exists, we'll grab the first IP from it.
    %% Otherwise, we'll fall back to Cowboy's reported peer IP.
    XForwardedFor = cowboy_req:header(<<"x-forwarded-for">>, Req, <<>>),
    new(parse_forwarded_ip(XForwardedFor, PeerIpAddress)).

%%====================================================================
%% Internal functions
%%====================================================================

-spec new(inet:ip_address()) -> req().
new(IpAddress) ->
    #{
        client_ip => IpAddress
    }.

-spec parse_forwarded_ip(binary(), inet:ip_address()) -> inet:ip_address().
parse_forwarded_ip(<<>>, Default) -> Default;
parse_forwarded_ip(XForwardedFor, Default) ->
    parse_split_forwarded_ip(string:split(XForwardedFor, <<",">>), Default).

-spec parse_split_forwarded_ip([string()], inet:ip_address()) -> inet:ip_address().
parse_split_forwarded_ip([<<>>], Default) -> Default;
parse_split_forwarded_ip([FirstIp|_], Default) ->
    case inet:parse_address(binary_to_list(FirstIp)) of
        {ok, IpAddress} ->
            IpAddress;
        {error, einval} ->
            Default
    end.
