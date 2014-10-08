-module(vodkatv).

-compile(export_all).

-record(state, {token}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
initial_state() ->
    #state {
        token = undefined
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% link_permitted
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
link_permitted(_Super, State, Link) ->
    PrivateState = js_links_machine:private_state(State),
    LinkTitle = jsg_links:link_title(Link),
    case {PrivateState#state.token, LinkTitle} of
        {undefined, "login"} ->
            true;
        {_T, "login"} ->
            false;
        {undefined, _L} ->
            false;
        {_T, _L} ->
            true
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% next_state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
next_state(Super, State, Result, Call) ->

    LinkTitle = js_links_machine:call_link_title(Call),
    PrivateState = js_links_machine:private_state(State),
    JSONResult = js_links_machine:get_json_body(Result),

    NextPrivateState = next_state_internal(PrivateState, JSONResult, LinkTitle), 

    Super(js_links_machine:set_private_state(NextPrivateState, State),
        Result, Call).

next_state_internal(PrivateState, JSONResult, "login") ->
    Token = jsg_jsonschema:propertyValue(JSONResult, "token"),
    PrivateState#state {
        token = Token
    };

next_state_internal(PrivateState, _JSONResult, _LinkTitle) ->
    PrivateState.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% postcondition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
postcondition(Super, State, Call, Result) ->
    LinkTitle = js_links_machine:call_link_title(Call),
    PrivateState = js_links_machine:private_state(State),
    JSONResult = js_links_machine:get_json_body(Result),
    io:format("* postcondition: ~p => ~p ~n", [LinkTitle, JSONResult]),
    postcondition_internal(PrivateState, JSONResult, LinkTitle) andalso
        Super(State, Call, Result).

postcondition_internal(_PrivateState, _JSONResult, "login") ->
    true;

postcondition_internal(_PrivateState, _JSONResult, _LinkTitle) ->
    true.
