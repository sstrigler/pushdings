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

-define(METHODS, [<<"OPTIONS">>, <<"GET">>]).

init(_, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, State) -> pushdings_rest:rest_init(Req, State, ?METHODS).

allowed_methods(Req, State) -> {?METHODS, Req, State}.

is_authorized(Req, State) ->
    pushdings_rest:is_authorized(Req, State,
                                 fun pushdings_account:check_password/2).

forbidden(Req, Id) -> pushdings_rest:forbidden(Req, Id).

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

resource_exists(Req0, Id) ->
    try
        {AppId, Req1} = cowboy_req:binding(app_id, Req0),
        App = pushdings_application:read(AppId),
        {true, Req1, App}
    catch
        _:{badmatch, _} ->
            {false, Req0, Id}
    end.

%% ------------------------------- < internal > --------------------------------

to_json(Req, App) -> {jsx:encode(App), Req, App}.
