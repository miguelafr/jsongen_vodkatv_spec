-module(test).
-compile(export_all).

test1() ->
  js_links_machine:run_statem(vodkatv,["login.jsch","login_response.jsch",
        "channels_response.jsch", "channel.jsch"]).

test1(N) ->
  js_links_machine:run_statem(vodkatv,["login.jsch","login_response.jsch",
        "channels_response.jsch", "channel.jsch"],
    [{timeout,N},{show_http_timing,true},{show_uri,true}]).

test1(N,Options) ->
  js_links_machine:run_statem(vodkatv,["login.jsch","login_response.jsch",
        "channels_response.jsch", "channel.jsch"],
    [{timeout,N},{show_http_timing,true},{show_uri,true}|Options]).

