-module(pushding_app_db).

-export([init/0,
         check_token/2]).

-record(pushding_app, {app_id :: binary(),
                       app_token :: binary(),
                       app_auth_uri :: binary()}).

init() ->
    mnesia:create_table(pushding_app,
                        [{attributes, record_info(fields, pushding_app)}]).


check_token(AppId, Token) ->
    case mnesia:dirty_read(pushding_app, AppId) of
        [#pushding_app{app_token=Token}] -> true;
        _ -> false
    end.
