-module(pushdings).
-behaviour(application).

-export([publish/2,
         start/0,
         subscribe/1]).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_',
         [{"/ws",  pushdings_ws_handler,   []},
          {"/",    cowboy_static,          {priv_file, pushdings, "ws.html"}},
          {"/api", pushdings_rest_handler, []}]}
    ]),
    {ok, _Pid} = cowboy:start_http(pushdings_http_listener, 100,
                                   [{port, 8080}],
                                   [{env, [{dispatch, Dispatch}]}]
                                  ),
    pushdings_app:init(),
    pushdings_sup:start_link().

stop(_State) -> cowboy:stop_listener(pushdings_http_listener).

start() -> application:ensure_all_started(pushdings).

publish(Topic, Message) -> gproc_ps:publish(l, {?MODULE, Topic}, Message).

subscribe(Topic) -> gproc_ps:subscribe(l, {?MODULE, Topic}).
