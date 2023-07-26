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
    <link rel=\"stylesheet\" type=\"text/css\" href=\"assets/root.css\" />
</head>
<body>
    <div>
        <h2 style=\"text-align:center;\"><span style=\"color:#0f2557\">r</span><span style=\"color:#28559a\">i</span><span style=\"color:#3778c2\">n</span><span style=\"color:#4b9fe1\">s</span><span style=\"color:#63bce5\">e</span></h2>
        <form id=\"search\">
            <input id=\"question\" list=\"commands\" type=\"search\" autocapitalize=\"none\" size=\"30\" style=\"text-align:center\" />
            <datalist id=\"commands\">
                <option value=\"convert \"/>
                <option value=\"ddg \"/>
                <option value=\"ddgi \"/>
                <option value=\"ddgm \"/>
                <option value=\"ddgn \"/>
                <option value=\"ddgv \"/>
                <option value=\"define \"/>
                <option value=\"md5 \"/>
                <option value=\"now\"/>
                <option value=\"sha \"/>
                <option value=\"sha2 \"/>
                <option value=\"uuid\"/>
                <option value=\"what is my ip\"/>
                <option value=\"what is my user agent\"/>
                <option value=\"wiki \"/>
                <option value=\"how to \"/>
            </datalist>
        </form>
        <br/><br/>
        <div id=\"results\">
            <div id=\"answer\">
            </div>
        </div>
    </div>
<footer>
    <p><a href=\"/about\">About</a> - <a href=\"/commands\">Commands</a></p>
</footer>
</body>
</html>">>,
    {Body, Req, State}.

hello_to_json(Req, State) ->
    Body = <<"{\"rinse\": \"rinse is not a search engine.\"}">>,
    {Body, Req, State}.

hello_to_text(Req, State) ->
    Body = <<"rinse\nrinse is not a search engine.">>,
    {Body, Req, State}.
