-module(pushding_rest_handler).

-export([init/3,
         allowed_methods/2,
         content_types_accepted/2
        ]).

-export([from_json/2]).

init(_, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}],
     Req, State}.

from_json(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:body(Req0),
    try
        Message = jsx:decode(Body, [{labels, atom}, return_maps]),
        pushding:publish("test", Message),
        {true, Req1, State}
    catch
        _:_Error ->
            {false, Req1, State}
    end.
