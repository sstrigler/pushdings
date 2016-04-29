-module(pushdings_registration_handler).

-export([init/3,
         allowed_methods/2,
         resource_exists/2,
         content_types_accepted/2
        ]).

-export([from_json/2]).

init(_, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) -> {[<<"PUT">>], Req, State}.

resource_exists(Req0, _State) ->
    {Email, Req} = cowboy_req:binding(email, Req0, <<>>),
    {pushdings_registration:exists(Email), Req, Email}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

%% -----------------------------------------------------------------------------

from_json(Req0, Email) ->
    {ok, Body, Req1} = cowboy_req:body(Req0),
    try
        #{email := Email} = Json =
            jsx:decode(Body, [{labels, atom}, return_maps]),
        ok = pushdings_registration:confirm(Json),
        {true, Req1, Email}
    catch
        _:Error ->
            pushdings:debug("confirmation failed: ~p", [Error]),
            {false, Req1, Email}
    end.
