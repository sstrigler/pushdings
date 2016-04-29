-module(pushdings).
-behaviour(application).

-export([start/2, stop/1]).

-export([install/1,
         lift/1,
         num_subscriptions/1,
         publish/2,
         start/0,
         subscribe/1]).

-export([debug/1,
         debug/2,
         error/1,
         error/2,
         info/1,
         info/2,
         warning/1,
         warning/2]).

%% ----------------------------- < application > -------------------------------
start(_Type, _Args) ->
    mnesia:wait_for_tables(
      pushdings_application:tables() ++
          pushdings_registration:tables(), 5000),

    Dispatch = cowboy_router:compile([
        {'_', [
               {"/", cowboy_static, {priv_file, pushdings, "ws.html"}},
               {"/applications", pushdings_applications_handler, []},
               {"/applications/:id", pushdings_application_handler, []},
               {"/messages", pushdings_messages_handler, []},
               {"/registrations/:email", pushdings_registration_handler, []},
               {"/registrations", pushdings_registrations_handler, []},
               {"/ws", pushdings_ws_handler, []}
              ]}]),

    {ok, _Pid} = cowboy:start_http(pushdings_http_listener, 100,
                                   [{port, 8080}],
                                   [{env, [{dispatch, Dispatch}]}]
                                  ),

    pushdings_sup:start_link().

stop(_State) -> cowboy:stop_listener(pushdings_http_listener).

%% --------------------------------- < api > -----------------------------------
install(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    ok = application:start(mnesia),
    pushdings_application:install(Nodes),
    pushdings_registration:install(Nodes).


start() -> application:ensure_all_started(pushdings).

publish(Topic, Message) -> gproc_ps:publish(l, {?MODULE, Topic}, Message).

subscribe(Topic) -> gproc_ps:subscribe(l, {?MODULE, Topic}).

num_subscriptions(Topic) -> length(gproc_ps:list_subs(l, {?MODULE, Topic})).

lift(Error) when is_tuple(Error), element(1, Error) == error -> Error;
lift(Ok)    when is_tuple(Ok), element(1, Ok) == ok          -> Ok;
lift(Other)                                                     -> {ok, Other}.

%% -------------------------------- < lager > ----------------------------------
-define(LAGER1(F), F(X)    -> ok = lager:F(X)).
-define(LAGER2(F), F(X, Y) -> ok = lager:F(X, Y)).

?LAGER1(debug).
?LAGER2(debug).
?LAGER1(error).
?LAGER2(error).
?LAGER1(info).
?LAGER2(info).
?LAGER1(warning).
?LAGER2(warning).
