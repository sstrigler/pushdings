-module(pushdings_application).

-export([create/2,
         get_auth_uri/1,
         get_max_clients/1,
         install/1,
         is_token_valid/2,
         read/1,
         set_auth_uri/2,
         set_token/2,
         set_max_clients/2,
         tables/0]).

-define(TAB, pushdings_application).

-record(?TAB,
        {id              :: binary(),
         token           :: binary(),
         auth_uri = <<>> :: binary(),
         max_clients = 3 :: pos_integer()}).

%% -----------------------------------------------------------------------------

-spec create(AppId :: binary(), Token :: binary()) -> ok.
create(AppId, Token) when is_binary(AppId), AppId /= <<>>,
                              is_binary(Token), Token /= <<>> ->
    F = fun() ->
                [] = mnesia:read(?TAB, AppId),
                MaxClients = pushdings:config(max_clients_default, 3),
                mnesia:write(#?TAB{
                                id          = AppId,
                                token       = crypto:hash(sha256, Token),
                                max_clients = MaxClients})
        end,
    {atomic, ok} = mnesia:transaction(F),
    ok.

%% -----------------------------------------------------------------------------

-spec get_auth_uri(AppId :: binary()) -> binary().
get_auth_uri(AppId) -> get_prop(AppId, #?TAB.auth_uri).

%% -----------------------------------------------------------------------------

-spec get_max_clients(AppId :: binary()) -> non_neg_integer().
get_max_clients(AppId) -> get_prop(AppId, #?TAB.max_clients).

%% -----------------------------------------------------------------------------

-spec install(Nodes :: list(atom())) -> ok.
install(Nodes) ->
    {atomic, ok} = mnesia:create_table(
                     ?TAB,
                     [{disc_copies, Nodes},
                      {attributes, record_info(fields, ?TAB)}]),
    ok.

%% -----------------------------------------------------------------------------

-spec is_token_valid(AppId :: binary(), Token :: binary()) -> boolean().
is_token_valid(AppId, Token) ->
    crypto:hash(sha256, Token) == get_prop(AppId, #?TAB.token).

%% -----------------------------------------------------------------------------

-spec read(AppId :: binary()) -> map().
%% [TODO] add type spec for map
read(AppId) ->
    [#?TAB{
         id          = Id,
         auth_uri    = Uri,
         max_clients = Clients}] =
        mnesia:dirty_read(?TAB, AppId),
    #{app_id      => Id,
      auth_uri    => Uri,
      max_clients => Clients}.

%% -----------------------------------------------------------------------------

-spec set_auth_uri(AppId :: binary(), Uri :: binary()) -> ok.
set_auth_uri(AppId, Uri) when is_binary(Uri) ->
    [App] = mnesia:dirty_read(?TAB, AppId),
    mnesia:dirty_write(App#?TAB{auth_uri = Uri}).

%% -----------------------------------------------------------------------------

-spec set_token(AppId :: binary(), Token :: binary()) -> ok.
set_token(AppId, Token) when is_binary(Token), Token /= <<>> ->
    [App] = mnesia:dirty_read(pushdings_appliction, AppId),
    mnesia:dirty_write(App#?TAB{token = crypto:hash(sha256, Token)}).

%% -----------------------------------------------------------------------------

-spec set_max_clients(AppId :: binary(), MaxClient :: pos_integer()) -> ok.
set_max_clients(AppId, MaxClients) when is_integer(MaxClients),
                                        MaxClients >= 0 ->
    [App] = mnesia:dirty_read(?TAB, AppId),
    mnesia:dirty_write(App#?TAB{max_clients = MaxClients}).

%% -----------------------------------------------------------------------------

-spec tables() -> list(atom()).
tables() -> [?TAB].

%% ------------------------------ < internal > ---------------------------------

-spec get_prop(AppId:: binary(), PropIdx :: pos_integer()) -> term().
get_prop(AppId, PropIdx) ->
    [App] =  mnesia:dirty_read(?TAB, AppId),
    element(PropIdx, App).
