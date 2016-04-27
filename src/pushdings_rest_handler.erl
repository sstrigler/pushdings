-module(pushdings_rest_handler).

-export([init/3,
         allowed_methods/2,
         forbidden/2,
         content_types_accepted/2
        ]).

-export([from_json/2]).

init(_, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

forbidden(Req0, State) ->
    case get_app_token(Req0) of
        {not_found, Req1} ->
            {true, Req1, State};
        {AppId, Token, Req2} ->
            {not pushdings_app_db:check_token(AppId, Token),
             Req2, AppId}
    end.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}],
     Req, State}.

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
    case cowboy_req:header(<<"pushdings_app_id">>, Req0) of
        {undefined, Req1} ->
            {not_found, Req1};
        {AppId, Req1} ->
            case cowboy_req:header(<<"pushdings_app_token">>, Req1) of
                {undefined, Req2} ->
                    {not_found, Req2};
                {Token, Req2} ->
                    {AppId, Token, Req2}
            end
    end.