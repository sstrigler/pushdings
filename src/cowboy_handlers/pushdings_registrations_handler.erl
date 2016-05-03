-module(pushdings_registrations_handler).

-export([init/3,
         rest_init/2,
         allowed_methods/2,
         content_types_accepted/2
        ]).

-export([from_json/2]).

-define(METHODS, [<<"OPTIONS">>, <<"POST">>]).

init(_, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, State) -> pushdings_rest:rest_init(Req, State, ?METHODS).

allowed_methods(Req, State) -> {?METHODS, Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

%% -----------------------------------------------------------------------------

from_json(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:body(Req0),
    try
        Json = jsx:decode(Body, [{labels, atom}, return_maps]),
        Id = pushdings_registration:create(Json),
        {Uri, Req2} = cowboy_req:url(Req1),
        {{true, uri(Uri, Id)}, Req2, State}
    catch
        _:_Error ->
            {false, Req1, State}
    end.

uri(Uri0, Id) ->
    IdEncoded = list_to_binary(http_uri:encode(binary_to_list(Id))),
    <<Uri0/binary, "/", IdEncoded/binary>>.
