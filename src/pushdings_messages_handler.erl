-module(pushdings_messages_handler).

-export([init/3,
         rest_init/2,
         allowed_methods/2,
         is_authorized/2,
         content_types_accepted/2
        ]).

-export([from_json/2]).

init(_, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req0, State) ->
    case cowboy_req:header(<<"origin">>, Req0) of
        {undefined, Req1} -> {ok, Req1, State};
        {Origin, Req1}    ->
            %% set CORS headers
            Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>,
                                              Origin, Req1),
            Req3 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>,
                                              <<"POST, OPTIONS">>, Req2),
            Req4 = cowboy_req:set_resp_header(
                     <<"access-control-allow-credentials">>, <<"true">>, Req3),
            Req5 = cowboy_req:set_resp_header(
                     <<"access-control-allow-headers">>, <<"content-type">>, Req4),
            {ok, Req5, State}
    end.

allowed_methods(Req, State) -> {[<<"OPTIONS">>, <<"POST">>], Req, State}.

is_authorized(Req, State) ->
    case cowboy_req:method(Req) of
        %% no auth for OPTIONS due to CORS
        {<<"OPTIONS">>, Req0} -> {true, Req0, State};
        {_Any,          Req0} ->
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
            end
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
