%%%-------------------------------------------------------------------
%% @doc rinseweb cache
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_cache).

%% API
-export([start/2]).
-export([get/2]).
-export([put/3]).

%% Types
-type options() :: #{
    type => ets:table_type(),          % set | ordered_set | bag | duplicate_bag
    n => pos_integer(),                % number of cache segments, default is 10
    ttl => pos_integer(),              % time to live of cached items in seconds, default is 600
    size => pos_integer(),             % number of items to store in cache
    memory => pos_integer(),           % rough number of bytes available to cache items
    policy => lru | mru,               % cache eviction policy, default is lru
    check => pos_integer(),            % time in seconds to enforce cache policy, default ttl / segments
    stats => fun() | {atom(), atom()}, % cache statistics handler
    heir => atom() | pid()             % heir of evicted cache segments
}.

-export_type([options/0]).

%%====================================================================
%% API
%%====================================================================

-spec start(atom(), options()) -> ok.
start(Name, Options) when is_atom(Name) ->
    CacheOpts = maps:to_list(Options),
    {ok, _} = cache:start_link(Name, CacheOpts),
    ok.

-spec get(atom(), any()) -> any() | undefined.
get(Name, Key) ->
    cache:get(Name, Key).

-spec put(atom(), any(), any()) -> ok.
put(Name, Key, Value) ->
    cache:put(Name, Key, Value).
