-module(pushding_ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3,
         terminate/3]).

-record(state, {
}).

init(_, _, _) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_, Req, _Opts) ->
    Req2 = cowboy_req:compact(Req),
    pushding:subscribe("test"),
    {ok, Req2, #state{}}.

%% websocket_handle({text, Data}, Req, State) ->
%%      {reply, {text, Data}, Req, State};
%% websocket_handle({binary, Data}, Req, State) ->
%%      {reply, {binary, Data}, Req, State};
websocket_handle(_Frame, Req, State) ->
     {ok, Req, State}.

websocket_info({gproc_ps_event, {pushding, "test"}, Message}, Req, State) ->
    {reply, {text, jsx:encode(Message)}, Req, State};
websocket_info(_Info, Req, State) ->
    io:format("unknown message recieved ~p~n", [_Info]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

terminate(_Reason, _Req, _State) ->
    gproc:goodbye(),
    ok.
