-module(pushdings_applications_handler).

-export([init/3,
         allowed_methods/2,
         forbidden/2,
         content_types_accepted/2
        ]).

-export([from_json/2]).

init(_, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) -> {[<<"POST">>], Req, State}.

forbidden(Req0, State) ->
    {RegId, Req1} = cowboy_req:header(<<"pushdings-reg-id">>, Req0, <<>>),
    {RegPw, Req2} = cowboy_req:header(<<"pushdings-reg-pw">>, Req1, <<>>),
    {not pushdings_registration:is_password_valid(RegId, RegPw), Req2, State}.

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
                     (pushdings_application:as_map(AppId))#{token => AppToken}),

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
