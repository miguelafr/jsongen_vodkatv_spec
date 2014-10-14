-module(test).
-compile(export_all).

test1() ->
  js_links_machine:run_statem(vodkatv,["login.jsch","login_response.jsch"]).

test1(N) ->
  js_links_machine:run_statem(vodkatv,["login.jsch","login_response.jsch"],[{timeout,N},{show_http_timing,true}]).

