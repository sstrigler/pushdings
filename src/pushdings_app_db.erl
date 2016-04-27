-module(pushdings_app_db).

-export([create_app/2,
         get_app_auth_uri/1,
         init/0,
         is_token_valid/2,
         set_app_auth_uri/2,
         set_app_token/2]).

-record(pushdings_app,
        {id              :: binary(),
         token           :: binary(),
         auth_uri = <<>> :: binary(),
         max_clients = 3 :: pos_integer()}).

%% ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-spec create_app(binary(), binary()) -> ok.
create_app(AppId, Token) when is_binary(AppId), AppId /= <<>>,
                              is_binary(Token), Token /= <<>> ->
    [] = mnesia:dirty_read(pushdings_app, AppId), % ensure not exists
    mnesia:dirty_write(#pushdings_app{id=AppId, token=Token}).

%% ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-spec get_app_auth_uri(binary()) -> {ok, binary()} | {error, not_found}.
get_app_auth_uri(AppId) ->
    case mnesia:dirty_read(pushdings_app, AppId) of
        [#pushdings_app{auth_uri=Uri}] -> {ok, Uri};
        []                             -> {error, not_found}
    end.

%% ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
%% [TODO] Persist table
init() -> mnesia:create_table(
            pushdings_app,
            [{attributes, record_info(fields, pushdings_app)}]).

%% ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-spec is_token_valid(binary(), binary()) -> boolean().
is_token_valid(AppId, Token) ->
    case mnesia:dirty_read(pushdings_app, AppId) of
        [#pushdings_app{token=Token}] -> true;
        _AnythingElse                 -> false
    end.

%% ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-spec set_app_auth_uri(binary(), binary()) -> ok.
set_app_auth_uri(AppId, Uri) when is_binary(Uri) ->
    [App] = mnesia:dirty_read(pushdings_app, AppId),
    mnesia:dirty_write(App#pushdings_app{auth_uri = Uri}).

%% ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
-spec set_app_token(binary(), binary()) -> ok.
set_app_token(AppId, Token) when is_binary(Token), Token /= <<>> ->
    [App] = mnesia:dirty_read(pushdings_app, AppId),
    mnesia:dirty_write(App#pushdings_app{token = Token}).
