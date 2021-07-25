-module(base_web_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([web_root_response_contains_form/1]).
-export([web_app_js_loads/1]).
-export([web_about_page_loads/1]).
-export([web_commands_page_loads/1]).

%% ============================================================================
%% ct functions
%% ============================================================================

all() ->
    [
        web_root_response_contains_form,
        web_app_js_loads,
        web_about_page_loads,
        web_commands_page_loads
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

web_root_response_contains_form(_) ->
    {ok, {{"HTTP/1.1", 200, "OK"}, _Headers, ResponseBody}} = httpc:request("http://localhost:8080"),
    FoundPos = string:str(ResponseBody, "<form id=\"search\">"),
    true = FoundPos > 0,
    ok.

web_app_js_loads(_) ->
    {ok, {{"HTTP/1.1", 200, "OK"}, _Headers, _ResponseBody}} = httpc:request("http://localhost:8080/assets/app.js"),
    ok.

web_about_page_loads(_) ->
    {ok, {{"HTTP/1.1", 200, "OK"}, _Headers, _ResponseBody}} = httpc:request("http://localhost:8080/about"),
    ok.

web_commands_page_loads(_) ->
    {ok, {{"HTTP/1.1", 200, "OK"}, _Headers, _ResponseBody}} = httpc:request("http://localhost:8080/commands"),
    ok.
