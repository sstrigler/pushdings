-module(pushdings_messages_handler).

-export([init/3,
         allowed_methods/2,
         is_authorized/2,
         content_types_accepted/2
        ]).

-export([from_json/2]).

init(_, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) -> {[<<"POST">>], Req, State}.

is_authorized(Req0, State) ->
    case cowboy_req:parse_header(<<"authorization">>, Req0) of
        {ok, {<<"basic">>, {AppId, Token}}, Req1} ->
            case pushdings_application:is_token_valid(AppId, Token) of
                true ->
                    %% AppId becomes State
                    {true, Req1, AppId};
                false -> {{false, <<"Basic realm=\"pushdings\"">>}, Req1, State}
            end;
        {ok, undefined, Req1} ->
            {{false, <<"Basic realm=\"pushdings\"">>}, Req1, State}
    end.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

%% -----------------------------------------------------------------------------

from_json(Req0, AppId) ->
    {ok, Body, Req1} = cowboy_req:body(Req0),
    try
        Message = jsx:decode(Body, [{labels, atom}, return_maps]),
        pushdings:publish(AppId, Message),
        {true, Req1, AppId}
    catch
        _:Error ->
            pushdings:debug("failed to send message with reason: ~p", [Error]),
            {false, Req1, AppId}
    end.
