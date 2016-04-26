-module(pushdings_ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3,
         terminate/3]).

init(_, _, _) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_, Req0, _Opts) ->
    Req = cowboy_req:compact(Req0),
    {ok, Req, none}.

websocket_handle({text, Data}, Req, State) ->
    io:format("got ~p", [Data]),
      {ok, Req, State};
%% websocket_handle({binary, Data}, Req, State) ->
%%      {reply, {binary, Data}, Req, State};
websocket_handle(_Frame, Req, State) ->
     {ok, Req, State}.

websocket_info({gproc_ps_event, {pushdings, _Topic}, Message}, Req, State) ->
    {reply, {text, jsx:encode(Message)}, Req,  State};
websocket_info(Info, Req, State) ->
    io:format("unknown message recieved ~p~n", [Info]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

terminate(_Reason, _Req, _State) ->
    gproc:goodbye(),
    ok.
