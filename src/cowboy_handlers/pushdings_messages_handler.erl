-module(pushdings_messages_handler).

-export([init/3,
         rest_init/2,
         allowed_methods/2,
         is_authorized/2,
         content_types_accepted/2
        ]).

-export([from_json/2]).

-define(METHODS, [<<"OPTIONS">>, <<"POST">>]).

init(_, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, State) -> pushdings_rest:rest_init(Req, State, ?METHODS).

allowed_methods(Req, State) -> {?METHODS, Req, State}.

is_authorized(Req, State) ->
    pushdings_rest:is_authorized(Req, State,
                                 fun pushdings_application:is_token_valid/2).

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
