-module(vodkatv).

-compile(export_all).

-record(channel, {vodkatvChannelId, number, name}).
-record(state, {token, channels, favorite_channels_ids}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
initial_state() ->
    #state {
        token = undefined,
        channels = undefined,
        favorite_channels_ids = []
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
compose_alternatives(_,_State,Alternatives) ->
  jsg_links_utils:freq_alternatives
    ([{1,whatever,"Login.do"},
      {2,whatever,"Logout.do"},
      {5,whatever,""}],
     Alternatives).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% link_permitted
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
link_permitted(_Super, State, Link) ->
    PrivateState = jsg_links_utils:private_state(State),
    LinkTitle = jsg_links:link_title(Link),
  case LinkTitle of
    "event" ->
      %io:format("Event event: link=~n~p~n",[Link]);
      ok;
    _ ->
      ok
  end,
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
    {_, _, follow_link, [_, {URI, RequestType, Body, QueryParms}], _} = Call, 
    LinkTitle = js_links_machine:call_link_title(Call),
    PrivateState = jsg_links_utils:private_state(State),
    JSONResult = js_links_machine:get_json_body(Result),

    NextPrivateState = next_state_internal(PrivateState,
        {URI,RequestType,Body,QueryParms}, JSONResult, LinkTitle), 

    NewState = jsg_links_utils:set_private_state(NextPrivateState, State),

    case LinkTitle of
      "logout" ->
	jsg_links_utils:remove_dynamic_links(NewState);
      _ ->
	Super(NewState, Result, Call)
    end.

next_state_internal(PrivateState, {_URI, _RequestType, _Body, _QueryParms},
        JSONResult, "login") ->
    Token = jsg_jsonschema:propertyValue(JSONResult, "token"),
    PrivateState#state {
        token = erlang:binary_to_list(Token)
    };

next_state_internal(PrivateState, {_URI, _RequestType, _Body, _QueryParms},
        JSONResult, "channels") ->
    Channels = jsg_jsonschema:propertyValue(JSONResult, "channels"),
    Elements = jsg_jsonschema:propertyValue(Channels, "elements"),
    PrivateState#state {
        channels = get_channels(Elements)
    };

next_state_internal(PrivateState, {_URI, _RequestType, _Body, QueryParms},
        _JSONResult, "add_to_favorite_channels") ->

    QueryParamVodkatvChannelId = proplists:get_value("vodkatvChannelId",
        QueryParms),

    case lists:member(QueryParamVodkatvChannelId,
            PrivateState#state.favorite_channels_ids) of
        false ->
            PrivateState#state {
                favorite_channels_ids = [QueryParamVodkatvChannelId |
                    PrivateState#state.favorite_channels_ids]
            };
        true ->
            PrivateState
    end;

next_state_internal(PrivateState, {_URI, _RequestType, _Body, QueryParms},
        _JSONResult, "remove_favorite_channel") ->

    QueryParamVodkatvChannelId = proplists:get_value("vodkatvChannelId",
        QueryParms),

    PrivateState#state {
        favorite_channels_ids = lists:delete(QueryParamVodkatvChannelId,
                PrivateState#state.favorite_channels_ids)
    };

next_state_internal(PrivateState, {_URI, _RequestType, _Body, _QueryParms},
        _JSONResult, "logout") ->
    PrivateState#state {
        token = undefined
    };

next_state_internal(PrivateState, {_URI, _RequestType, _Body, _QueryParms},
        _JSONResult, _LinkTitle) ->
    PrivateState.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% postcondition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
postcondition(Super, State, Call, Result) ->
    {_, _, follow_link, [_, {URI, RequestType, Body, QueryParms}], _} = Call, 
    LinkTitle = js_links_machine:call_link_title(Call),
    PrivateState = jsg_links_utils:private_state(State),
    case js_links_machine:validate_call_not_error_result(Call,Result) of
      true -> 
	case js_links_machine:response_has_body(Result) of
	  true ->
	    JSONResult = js_links_machine:get_json_body(Result),
	    postcondition_internal(PrivateState, {URI,RequestType,Body,QueryParms},
            JSONResult, LinkTitle) andalso Super(State, Call, Result);
	  false -> false
	end;
      false -> false
    end.

postcondition_internal(PrivateState, {_URI, _RequestType, _Body, _QueryParms},
        JSONResult, "login") ->
  io:format("hello ~p~n",[JSONResult]),
  io:format("now = ~p~n",[get_now()]),
    ValidUntil = jsg_jsonschema:propertyValue(JSONResult, "validUntil"),
    PrivateState#state.token =/= "" andalso ValidUntil > get_now();

postcondition_internal(PrivateState, {_URI, _RequestType, _Body, QueryParms},
        JSONResult, "channel") ->
    
    % Get channels from the state
    case PrivateState#state.channels of
        undefined ->
            % We can not check anything here if we don't have channels
            true;
        Channels ->
            % Get params
            QueryParamVodkatvChannelId = proplists:get_value("vodkatvChannelId", QueryParms),

            % Search channel in the state and compare
            case [Channel || Channel <- Channels,
                    Channel#channel.vodkatvChannelId == QueryParamVodkatvChannelId] of
                [Channel] ->
                    Channel == get_channel(
                        jsg_jsonschema:propertyValue(JSONResult, "channel"));
                _ ->
                    false
            end
    end;

postcondition_internal(_PrivateState, {_URI, _RequestType, _Body, _QueryParms},
        JSONResult, "languages") ->
    Languages = jsg_jsonschema:propertyValue(JSONResult, "languages"),
    Elements = jsg_jsonschema:propertyValue(Languages, "elements"),
    erlang:length(Elements) >= 0;

postcondition_internal(_PrivateState, {_URI, _RequestType, _Body, _QueryParms},
        JSONResult, "countries") ->
    Countries = jsg_jsonschema:propertyValue(JSONResult, "countries"),
    Elements = jsg_jsonschema:propertyValue(Countries, "elements"),
    erlang:length(Elements) >= 0;

postcondition_internal(_PrivateState, {_URI, _RequestType, _Body, _QueryParms},
        JSONResult, "categories") ->
    Categories = jsg_jsonschema:propertyValue(JSONResult, "categories"),
    Elements = jsg_jsonschema:propertyValue(Categories, "elements"),
    erlang:length(Elements) >= 0;

postcondition_internal(PrivateState, {_URI, _RequestType, _Body, _QueryParms},
        JSONResult, LinkTitle) when LinkTitle =:= "favorite_channels" ->
    Channels = jsg_jsonschema:propertyValue(JSONResult, "channels"),
    Elements = jsg_jsonschema:propertyValue(Channels, "elements"),
    FavoriteChannelIds = PrivateState#state.favorite_channels_ids,

    FavoriteChannelIdsFromResponse = lists:map(fun(Element) ->
        Channel = get_channel(Element),
        Channel#channel.vodkatvChannelId
    end, Elements),

    lists:all(fun(FavoriteChannelId) ->
        lists:member(FavoriteChannelId, FavoriteChannelIdsFromResponse)
    end, FavoriteChannelIds);

postcondition_internal(_PrivateState, {_URI, _RequestType, _Body, QueryParms},
        JSONResult, LinkTitle) when LinkTitle =:= "add_to_favorite_channels" ->
    QueryParamVodkatvChannelId = proplists:get_value("vodkatvChannelId",
        QueryParms),
    Channels = jsg_jsonschema:propertyValue(JSONResult, "channels"),
    Elements = jsg_jsonschema:propertyValue(Channels, "elements"),
    FavoriteChannelIdsFromResponse = lists:map(fun(Element) ->
        Channel = get_channel(Element),
        Channel#channel.vodkatvChannelId
    end, Elements),
    lists:member(QueryParamVodkatvChannelId, FavoriteChannelIdsFromResponse);

postcondition_internal(_PrivateState, {_URI, _RequestType, _Body, QueryParms},
        JSONResult, LinkTitle) when LinkTitle =:= "remove_favorite_channel" ->

    QueryParamVodkatvChannelId = proplists:get_value("vodkatvChannelId",
        QueryParms),
    Channels = jsg_jsonschema:propertyValue(JSONResult, "channels"),
    Elements = jsg_jsonschema:propertyValue(Channels, "elements"),
    FavoriteChannelIdsFromResponse = lists:map(fun(Element) ->
        Channel = get_channel(Element),
        Channel#channel.vodkatvChannelId
    end, Elements),
    not lists:member(QueryParamVodkatvChannelId, FavoriteChannelIdsFromResponse);

postcondition_internal(_PrivateState, {_URI, _RequestType, _Body, _QueryParms},
        JSONResult, "logout") ->
    JSONResult == {struct,[]};

postcondition_internal(_PrivateState, _Call, _JSONResult, _LinkTitle) ->
    true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_channel(Element) ->
    #channel {
        vodkatvChannelId = erlang:binary_to_list(
            jsg_jsonschema:propertyValue(Element, "vodkatvChannelId")),
        number = jsg_jsonschema:propertyValue(Element, "number"),
        name = erlang:binary_to_list(
            jsg_jsonschema:propertyValue(Element, "name"))
    }.

get_channels([]) ->
    [];
get_channels([Element | Elements]) ->
    Channel = get_channel(Element),
    [Channel | get_channels(Elements)].

get_now() ->
    {MegaSecs, Secs, _MicroSecs} = erlang:now(),
    (MegaSecs * 1000000 + Secs)*1000.
