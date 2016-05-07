-module(pushdings_account_confirmations_handler).

-export([init/3,
         rest_init/2,
         allowed_methods/2,
         is_authorized/2,
         forbidden/2,
         resource_exists/2,
         content_types_accepted/2
        ]).

-export([from_json/2]).

-define(METHODS, [<<"OPTIONS">>, <<"POST">>]).

init(_, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, State) -> pushdings_rest:rest_init(Req, State, ?METHODS).

allowed_methods(Req, State) -> {?METHODS, Req, State}.

is_authorized(Req, State) ->
    pushdings_rest:is_authorized(
      Req, State, fun pushdings_account:is_password_valid/2).

forbidden(Req, RegId) -> pushdings_rest:forbidden(Req, RegId).

resource_exists(Req, RegId) ->
    {pushdings_account:exists(RegId), Req, RegId}.

content_types_accepted(Req, RegId) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, RegId}.

%% -----------------------------------------------------------------------------

from_json(Req0, RegId) ->
    {ok, Body, Req1} = cowboy_req:body(Req0),
    try
        #{email := RegId,
          token := Token} = jsx:decode(Body, [{labels, atom}, return_maps]),
        ok = pushdings_account:confirm(RegId, Token),
        {true, Req1, RegId}
    catch
        _:Error ->
            pushdings:debug("confirmation failed: ~p", [Error]),
            {false, Req1, RegId}
    end.
