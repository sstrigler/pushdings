-module(pushdings_messages_handler).

-export([init/3,
         allowed_methods/2,
         forbidden/2,
         content_types_accepted/2
        ]).

-export([from_json/2]).

init(_, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) -> {[<<"POST">>], Req, State}.

forbidden(Req0, _State) ->
    {AppId, Token, Req1} = get_app_token(Req0), 
    %% AppId becomes State
    {not pushdings_app:is_token_valid(AppId, Token), Req1, AppId}.

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
        _:_Error ->
            {false, Req1, AppId}
    end.

%% -----------------------------------------------------------------------------

get_app_token(Req0) ->
    {AppId, Req1} = cowboy_req:header(<<"pushdings-app-id">>, Req0, <<>>),
    {Token, Req2} = cowboy_req:header(<<"pushdings-app-token">>, Req1, <<>>),
    {AppId, Token, Req2}.
