-module(pushdings_app).

-export([create/2,
         get_auth_uri/1,
         get_max_clients/1,
         init/0,
         is_token_valid/2,
         set_auth_uri/2,
         set_token/2,
         set_max_clients/2]).

-record(pushdings_app,
        {id              :: binary(),
         token           :: binary(),
         auth_uri = <<>> :: binary(),
         max_clients = 3 :: pos_integer()}).

%% ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-spec create(binary(), binary()) -> ok.
create(AppId, Token) when is_binary(AppId), AppId /= <<>>,
                              is_binary(Token), Token /= <<>> ->
    F = fun() ->
                [] = mnesia:read(pushdings_app, AppId), % ensure not exists
                mnesia:write(#pushdings_app{id=AppId, token=Token})
        end,
    {atomic, ok} = mnesia:transaction(F),
    ok.

%% ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-spec get_auth_uri(binary()) -> {ok, binary()} | {error, not_found}.
get_auth_uri(AppId) -> get_prop(AppId, #pushdings_app.auth_uri).

%% ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-spec get_max_clients(binary()) -> non_neg_integer().
get_max_clients(AppId) -> get_prop(AppId, #pushdings_app.max_clients).

%% ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
init() ->
    _ = mnesia:stop(),
    _ = mnesia:create_schema([node()]),
    _ = mnesia:start(),
    mnesia:create_table(
      pushdings_app,
      [{disc_copies, [node()]},
       {attributes, record_info(fields, pushdings_app)}]).


%% ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-spec is_token_valid(binary(), binary()) -> boolean().
is_token_valid(AppId, Token) ->
    {ok, Token} == get_prop(AppId, #pushdings_app.token).

%% ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-spec set_auth_uri(binary(), binary()) -> ok.
set_auth_uri(AppId, Uri) when is_binary(Uri) ->
    [App] = mnesia:dirty_read(pushdings_app, AppId),
    mnesia:dirty_write(App#pushdings_app{auth_uri = Uri}).

%% ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-spec set_token(binary(), binary()) -> ok.
set_token(AppId, Token) when is_binary(Token), Token /= <<>> ->
    [App] = mnesia:dirty_read(pushdings_app, AppId),
    mnesia:dirty_write(App#pushdings_app{token = Token}).

%% ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-spec set_max_clients(binary(), pos_integer()) -> ok.
set_max_clients(AppId, MaxClients) when is_integer(MaxClients),
                                        MaxClients >= 0 ->
    [App] = mnesia:dirty_read(pushdings_app, AppId),
    mnesia:dirty_write(App#pushdings_app{max_clients = MaxClients}).

%% ----- ----- ----- ----- -----  < internal > --- ----- ----- ----- ----- -----
-spec get_prop(binary(), pos_integer()) -> {ok, term()} | {error, not_found}.
get_prop(AppId, PropIdx) ->
    case mnesia:dirty_read(pushdings_app, AppId) of
        [App] -> {ok, element(PropIdx, App)};
        []    -> {error, not_found}
    end.
