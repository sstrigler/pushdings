-module(pushdings_registration_handler).

-export([init/3,
         allowed_methods/2,
         is_authorized/2,
         forbidden/2,
         resource_exists/2,
         content_types_accepted/2
        ]).

-export([from_json/2]).

init(_, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) -> {[<<"PUT">>], Req, State}.

is_authorized(Req0, State) ->
    case cowboy_req:parse_header(<<"authorization">>, Req0) of
        {ok, {<<"basic">>, {RegId, RegPw}}, Req1} ->
            case pushdings_registration:is_password_valid(RegId, RegPw) of
                true  -> {true, Req1, RegId};
                false -> {{false, <<"Basic realm=\"pushdings\"">>}, Req1, State}
            end;
        {ok, undefined, Req1} ->
            {{false, <<"Basic realm=\"pushdings\"">>}, Req1, State}
    end.

forbidden(Req0, RegId) ->
    case cowboy_req:binding(id, Req0, <<>>) of
        {RegId, Req} -> {false, Req, RegId};
        {_,     Req} -> {true,  Req, RegId}
    end.

resource_exists(Req, RegId) ->
    {pushdings_registration:exists(RegId), Req, RegId}.

content_types_accepted(Req, RegId) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, RegId}.

%% -----------------------------------------------------------------------------

from_json(Req0, RegId) ->
    {ok, Body, Req1} = cowboy_req:body(Req0),
    try
        #{token := Token} = jsx:decode(Body, [{labels, atom}, return_maps]),
        ok = pushdings_registration:confirm(RegId, Token),
        {true, Req1, RegId}
    catch
        _:Error ->
            pushdings:debug("confirmation failed: ~p", [Error]),
            {false, Req1, RegId}
    end.
