-module(pushding_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", pushding_rest_handler, []}]}
    ]),
    cowboy:start_http(pushding_http_listener, 100, [{port, 8080}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    pushding_sup:start_link().

stop(_State) ->
    ok.
