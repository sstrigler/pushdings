-module(pushdings_rest).

-export([
         rest_init/3,
         is_authorized/3,
         forbidden/2
        ]).

-define(AUTH_HEADER, <<"Basic realm=\"pushdings\"">>).

-include_lib("eunit/include/eunit.hrl").

%% -----------------------------------------------------------------------------

rest_init(Req, State, Methods) -> {ok, set_cors_header(Req, Methods), State}.

is_authorized(Req, State, CheckF) ->
    case cowboy_req:method(Req) of
        %% no auth for OPTIONS due to CORS
        {<<"OPTIONS">>, Req0} -> {true, Req0, State};
        {_Any,          Req0} ->
            case cowboy_req:parse_header(<<"authorization">>, Req0) of
                {ok, {<<"basic">>, {Id, Token}}, Req1} ->
                    case CheckF(Id, Token) of
                        true  -> {true, Req1, Id};
                        false -> {{false, ?AUTH_HEADER}, Req1, State}
                    end;
                {ok, undefined, Req1} ->
                    {{false, ?AUTH_HEADER}, Req1, State}
            end
    end.

forbidden(Req0, Id) ->
    case cowboy_req:method(Req0) of
        %% options is always allowed
        {<<"OPTIONS">>, Req0} -> {false, Req0, []};
        {_Any,          Req0} ->
            case cowboy_req:binding(id, Req0, <<>>) of
                {Id, Req2} -> {false, Req2, Id};
                {_,  Req2} -> {true,  Req2, []}
            end
    end.

%% ------------------------------ < internal > ---------------------------------

set_cors_header(Req0, Methods) ->
    case cowboy_req:header(<<"origin">>, Req0) of
        {undefined, Req1} -> Req1;
        {Origin, Req1}    ->
            %% set CORS headers
            Req2 = cowboy_req:set_resp_header(
                     <<"access-control-allow-origin">>,
                     Origin, Req1),
            Req3 = cowboy_req:set_resp_header(
                     <<"access-control-allow-methods">>,
                     bin_join(Methods), Req2),
            Req4 = cowboy_req:set_resp_header(
                     <<"access-control-allow-credentials">>,
                     <<"true">>, Req3),
            Req5 = cowboy_req:set_resp_header(
                     <<"access-control-allow-headers">>,
                     <<"authorization,content-type">>, Req4),
            Req5
    end.

bin_join(Methods) -> bin_join(Methods, <<>>).

bin_join([], Result)  -> Result;
bin_join([H|T], <<>>) -> bin_join(T, <<H/binary>>);
bin_join([H|T], Acc)  -> bin_join(T, <<Acc/binary, ", ", H/binary>>). 

%% -------------------------------- < tests > ----------------------------------

bin_join_test() ->
    ?assertEqual(<<"A, B">>, bin_join([<<"A">>, <<"B">>])),
    ?assertEqual(<<"A">>, bin_join([<<"A">>])).





