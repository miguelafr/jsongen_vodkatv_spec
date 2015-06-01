-module(test).
-compile(export_all).

test1() ->
  ok = application:ensure_started(jsongen),
  js_links_machine:run_statem(vodkatv,["login.jsch","login_response.jsch",
        "channels_response.jsch", "channel.jsch"],
        [{timeout,5000}, {show_uri,true}, {validator,jesse_validator}]).

test1(N) ->
  ok = application:ensure_started(jsongen),
  js_links_machine:run_statem(vodkatv,["login.jsch","login_response.jsch",
        "channels_response.jsch", "channel.jsch"],
    [{timeout,N},{show_http_timing,true},{show_uri,true}]).

test1(N,Options) ->
  ok = application:ensure_started(jsongen),
  js_links_machine:run_statem(vodkatv,["login.jsch","login_response.jsch",
        "channels_response.jsch", "channel.jsch"],
    [{timeout,N},{show_http_timing,true},{show_uri,true}|Options]).

