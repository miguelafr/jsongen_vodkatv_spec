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
    PrivateState = jsg_links_utils:private_state(State),
    LinkTitle = jsg_links:link_title(Link),
    %%io:format("link_permitted => token: ~p | title: ~p~n",
    %%	      [PrivateState#state.token, LinkTitle]),
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
    PrivateState = jsg_links_utils:private_state(State),
    JSONResult = js_links_machine:get_json_body(Result),

    NextPrivateState = next_state_internal(PrivateState, JSONResult, LinkTitle), 

    Super(jsg_links_utils:set_private_state(NextPrivateState, State),
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
    PrivateState = jsg_links_utils:private_state(State),
    case js_links_machine:validate_call_not_error_result(Call,Result) of
      true -> 
	case js_links_machine:response_has_body(Result) of
	  true ->
	    JSONResult = js_links_machine:get_json_body(Result),
	    postcondition_internal(PrivateState, JSONResult, LinkTitle) andalso
	      Super(State, Call, Result);
	  false -> false
	end;
      false -> false
    end.

postcondition_internal(_PrivateState, _JSONResult, "login") ->
    true;

postcondition_internal(_PrivateState, _JSONResult, _LinkTitle) ->
    true.
