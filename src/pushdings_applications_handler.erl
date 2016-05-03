-module(pushdings_applications_handler).

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
    pushdings_rest:is_authorized(
      Req, State, fun pushdings_registration:is_password_valid/2).

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

%% -----------------------------------------------------------------------------

from_json(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:body(Req0),
    try
        Json = json_decode(Body),

        AppId = gen_uuid(),
        AppToken = gen_uuid(),

        pushdings_application:create(AppId, AppToken),

        AuthUri = maps:get(auth_uri, Json, <<>>),
        pushdings_application:set_auth_uri(AppId, AuthUri),

        {Uri, Req2} = cowboy_req:url(Req1),

        RespBody = jsx:encode(
                     (pushdings_application:read(AppId))#{token => AppToken}),

        Req3 = cowboy_req:set_resp_body(RespBody, Req2),

        {{true, uri(Uri, AppId)}, Req3, State}
    catch
        _:Error ->
            pushdings:debug("creating app failed: ~p", [Error]),
            {false, Req1, State}
    end.

gen_uuid() ->
    list_to_binary(
      uuid:uuid_to_string(
        uuid:get_v4())).

json_decode(<<>>) -> #{};
json_decode(Body) -> jsx:decode(Body, [{labels, atom}, return_maps]).

uri(Uri0, Id) ->
    IdEncoded = list_to_binary(http_uri:encode(binary_to_list(Id))),
    <<Uri0/binary, "/", IdEncoded/binary>>.
