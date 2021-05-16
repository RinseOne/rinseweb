%%%-------------------------------------------------------------------
%% @doc rinseweb root handler
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_h_root).

%% API
-export([init/2]).
-export([content_types_provided/2]).
-export([hello_to_html/2]).
-export([hello_to_json/2]).
-export([hello_to_text/2]).

%%====================================================================
%% API
%%====================================================================

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {[
        {<<"text/html">>, hello_to_html},
        {<<"application/json">>, hello_to_json},
        {<<"text/plain">>, hello_to_text}
    ], Req, State}.

hello_to_html(Req, State) ->
    Nonce = base64:encode(crypto:strong_rand_bytes(15)),
    Body = <<"<html>
<head>
	<meta charset=\"utf-8\">
	<!-- <meta http-equiv=\"Content-Security-Policy\" content=\"default-src 'self'\"> -->
	<!-- <meta http-equiv=\"Content-Security-Policy\" content=\"script-src 'self' 'nonce-",Nonce/binary,"'\"> -->
	<meta http-equiv=\"Content-Security-Policy\" content=\"script-src 'nonce-",Nonce/binary,"'\">
	<title>rinse</title>
    <script type=\"text/javascript\" src=\"/assets/app.js\" nonce=\"",Nonce/binary,"\"></script>
    <style type=\"text/css\">
        h2 {font-size:5em;font-family: monospace;}
        input[type=\"text\"] {font-size:3em;}
        #answer {font-size:2em;}
    </style>
</head>
<body>
    <center>
        <h2><span style=\"color:#0f2557\">r</span><span style=\"color:#28559a\">i</span><span style=\"color:#3778c2\">n</span><span style=\"color:#4b9fe1\">s</span><span style=\"color:#63bce5\">e</span></h2>
        <form id=\"search\">
            <input id=\"question\" type=\"text\" size=\"30\" style=\"text-align:center\" />
        </form>
        <br/>
        <div id=\"answer\">
        </div>
    </center>
</body>
</html>">>,
    {Body, Req, State}.

hello_to_json(Req, State) ->
    Body = <<"{\"rest\": \"Hello World!\"}">>,
    {Body, Req, State}.

hello_to_text(Req, State) ->
    {<<"REST Hello World as text!">>, Req, State}.
