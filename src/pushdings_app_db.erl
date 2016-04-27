-module(pushdings_app_db).

-export([create_app/2,
         init/0,
         is_token_valid/2]).

-record(pushdings_app,
        {app_id       :: binary(),
         app_token    :: binary(),
         app_auth_uri :: binary()}).

create_app(AppId, Token) when is_binary(AppId) and is_binary(Token) ->
    mnesia:dirty_write(#pushdings_app{app_id=AppId, app_token=Token});
create_app(AppId, Token) ->
    create_app(list_to_binary(AppId), list_to_binary(Token)).

init() -> mnesia:create_table(
            pushdings_app,
            [{attributes, record_info(fields, pushdings_app)}]).


is_token_valid(AppId, Token) ->
    case mnesia:dirty_read(pushdings_app, AppId) of
        [#pushdings_app{app_token=Token}] -> true;
        _AnythingElse                     -> false
    end.
