%%%-------------------------------------------------------------------
%% @doc rinseweb application
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", rinseweb_h_root, []},
            {"/answer/[:question]", rinseweb_h_answer, []},
            {"/about", cowboy_static, {priv_file, rinseweb, "static/html/about.html"}},
            {"/commands", cowboy_static, {priv_file, rinseweb, "static/html/commands.html"}},
            {"/assets/[...]", cowboy_static, {priv_dir, rinseweb, "static/assets"}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }),
    rinseweb_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http),
    ok.
