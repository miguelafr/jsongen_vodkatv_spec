-module(test).
-compile(export_all).

test1() ->
  js_links_machine:run_statem(void,["login.jsch","login_response.jsch"]).

