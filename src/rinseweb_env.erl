%%%-------------------------------------------------------------------
%% @doc rinseweb environment
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_env).

-export([port/0]).

-spec port() -> integer().
port() ->
    list_to_integer(os:getenv("RINSEWEB_PORT", "8080")).
