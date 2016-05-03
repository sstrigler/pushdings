-module(pushdings_application_handler).

-export([init/3,
         rest_init/2,
         allowed_methods/2,
         is_authorized/2,
         forbidden/2,
         content_types_provided/2,
         resource_exists/2
        ]).

-export([to_json/2]).

init(_, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, State) -> pushdings_rest:rest_init(Req, State).

allowed_methods(Req, State) -> {[<<"OPTIONS">>, <<"GET">>], Req, State}.

is_authorized(Req, State) ->
    pushdings_rest:is_authorized(Req, State,
                                 fun pushdings_application:is_token_valid/2).

forbidden(Req, AppId) -> pushdings_rest:forbidden(Req, AppId).

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

resource_exists(Req, AppId) ->
    try
        App = pushdings_application:read(AppId),
        {true, Req, App}
    catch
        _:{badmatch, _} ->
            {false, Req, AppId}
    end.

%% ------------------------------- < internal > --------------------------------

to_json(Req, App) -> {jsx:encode(App), Req, App}.
