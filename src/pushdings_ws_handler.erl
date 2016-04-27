-module(pushdings_ws_handler).
-behaviour(cowboy_websocket_handler).

-define(PROTOCOL, <<"pushdings_v1">>).

-export([init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3,
         terminate/3]).

init(_, _, _) -> {upgrade, protocol, cowboy_websocket}.

websocket_init(_Prot, Req0, _Opts) ->
    case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req0) of
        {ok, undefined, Req1} ->
            lager:debug("no protocols given, asuming default", []),
            Req2 = cowboy_req:compact(Req1),
            {ok, Req2, ?PROTOCOL, hibernate};
        {ok, Subprotocols, Req1} ->
            lager:debug("protocols: ~p", [Subprotocols]),
            case lists:member(?PROTOCOL, Subprotocols) of
                true ->
                    Req2 = cowboy_req:compact(Req1),
                    Req3 = cowboy_req:set_resp_header(
                             <<"sec-websocket-protocol">>,
                             ?PROTOCOL, Req2),
                    {ok, Req3, ?PROTOCOL, hibernate};
                false ->
                    lager:warning("No supported protocol found in ~p",
                                  [Subprotocols]),
                    {shutdown, Req1}
            end
    end.

websocket_handle({text, Data}, Req, ?PROTOCOL = State) ->
    lager:debug("Got data: ~p", [Data]),
    #{ app_id     := AppId,
       user_id    := UserId,
       user_token := UserToken } = Json = jsx:decode(
                                            Data,
                                            [return_maps, {labels, atom}]),
    case is_not_rate_limited(AppId) of
        true ->
            case is_valid_user(AppId, UserId, UserToken) of
                true  ->
                    pushdings:subscribe(AppId),
                    {ok, Req, State};
                false ->
                    lager:warning("Could not auth user: ~p", [Json]),
                    {shutdown, Req, State}
            end;
        false ->
            lager:warning("App ~p rate limited", [AppId]),
            {shutdown, Req, State}
    end;
websocket_handle(Frame, Req, State) ->
    lager:warning("Not handling unknown frame: ~p", [Frame]),
    {ok, Req, State}.

websocket_info({gproc_ps_event, {pushdings, _Topic}, Message}, Req, State) ->
    lager:debug("pushing ~p", [Message]),
    {reply, {text, jsx:encode(Message)}, Req, State};
websocket_info(Info, Req, State) ->
    lager:warning("unknown message received ~p~n", [Info]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) -> ok.

terminate(_Reason, _Req, _State) ->
    gproc:goodbye(),
    ok.

%% ----- ----- ----- ----- -----  < internal > --- ----- ----- ----- ----- -----

-spec is_valid_user(binary(), binary(), binary()) -> boolean().
is_valid_user(AppId, UserId, UserToken) ->
    Uri = pushdings_app:get_auth_uri(AppId),
    check_user(Uri, UserId, UserToken).

-spec check_user(binary(), binary(), binary()) -> boolean().
check_user(<<>>, _, _)              -> true;
check_user(Uri0, UserId, UserToken) ->
    Uri = <<Uri0/binary, "?user=", UserId/binary, "&token=", UserToken/binary>>,
    case hackney:request(get, Uri, [], <<>>, []) of
        {ok, 200, _Headers, Client} ->
            {ok, Body} = hackney:body(Client),
            Body == <<"1">>;
        _ -> false
    end.

-spec is_not_rate_limited(binary()) -> boolean().
is_not_rate_limited(AppId) ->
    pushdings_app:get_max_clients(AppId) > pushdings:num_subscriptions(AppId).
