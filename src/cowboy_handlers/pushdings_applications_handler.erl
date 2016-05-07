%%% ============================================================================
%%%
%%% Create applications
%%%
%%% ============================================================================
-module(pushdings_applications_handler).

-export([init/3,
         rest_init/2,
         allowed_methods/2,
         is_authorized/2,
         forbidden/2,
         content_types_accepted/2
        ]).

-export([from_json/2]).

-define(METHODS, [<<"OPTIONS">>, <<"POST">>]).

init(_, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, State) -> pushdings_rest:rest_init(Req, State, ?METHODS).

allowed_methods(Req, State) -> {?METHODS, Req, State}.

is_authorized(Req, State) ->
    pushdings_rest:is_authorized(
      Req, State, fun pushdings_account:check_password/2).

forbidden(Req, Id) -> pushdings_rest:forbidden(Req, Id).

content_types_accepted(Req, Id) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, Id}.

%% -----------------------------------------------------------------------------

from_json(Req0, Id) ->
    {ok, Body, Req1} = cowboy_req:body(Req0),
    try
        Json = json_decode(Body),

        AppId = gen_uuid(),
        AppToken = gen_uuid(),

        pushdings_application:create(AppId, AppToken),

        AuthUri = maps:get(auth_uri, Json, <<>>),
        pushdings_application:set_auth_uri(AppId, AuthUri),

        {Uri, Req2} = cowboy_req:url(Req1),

        {{true, uri(Uri, AppId)}, Req2, Id}
    catch
        _:Error ->
            pushdings:debug("creating app failed: ~p", [Error]),
            {false, Req1, Id}
    end.

%% -----------------------------------------------------------------------------

gen_uuid() ->
    list_to_binary(
      uuid:uuid_to_string(
        uuid:get_v4())).

json_decode(<<>>) -> #{};
json_decode(Body) -> jsx:decode(Body, [{labels, atom}, return_maps]).

uri(Uri0, Id) ->
    IdEncoded = list_to_binary(http_uri:encode(binary_to_list(Id))),
    <<Uri0/binary, "/", IdEncoded/binary>>.
