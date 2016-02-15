-module(pushding).
-behaviour(application).

-export([publish/2,
         start/0,
         subscribe/1]).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/ws", pushding_ws_handler, []},
               {"/", cowboy_static, {priv_file, pushding, "ws.html"}},
               {"/api", pushding_rest_handler, []}]}
    ]),
    cowboy:start_http(pushding_http_listener, 100, [{port, 8080}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    pushding_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(pushding_http_listener).

start() ->
    application:ensure_all_started(pushding).

publish(Topic, Message) ->
    gproc_ps:publish(l, {?MODULE, Topic}, Message).

subscribe(Topic) ->
    gproc_ps:subscribe(l, {?MODULE, Topic}).
