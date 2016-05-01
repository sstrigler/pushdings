-module(pushdings_registration).

-export([confirm/2,
         create/1,
         exists/1,
         is_password_valid/2]).

%% maintenance
-export([install/1,
         tables/0]).

-record(pushdings_registration,
        {
          email            :: binary(),
          password         :: binary(),
          token            :: binary(),
          confirmed_at = 0 :: non_neg_integer(),
          created_at       :: non_neg_integer()
        }).

%% -----------------------------------------------------------------------------

-spec confirm(Email :: binary(), Token :: binary()) -> ok.
confirm(Email, Token) ->
    F = fun() ->
                [Registration] = mnesia:read(pushdings_registration, Email),

                Token = list_to_binary(
                          uuid:uuid_to_string(
                            Registration#pushdings_registration.token,
                            nodash
                           )),
                mnesia:write(Registration#pushdings_registration{
                               confirmed_at = os:system_time()
                              })
        end,
    {atomic, ok} = mnesia:transaction(F),
    ok.

%% ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----

-spec create(#{email => binary(), password => binary()}) -> binary().
create(#{email := Email, password := Password})
  when Email /= <<>>, Password /= <<>> ->
    F = fun() ->
                [] = mnesia:read(pushdings_registration, Email),
                Token = uuid:get_v4(),
                Registration =
                    #pushdings_registration{
                       email      = Email,
                       password   = crypto:hash(sha256, Password),
                       token      = Token,
                       created_at = os:system_time()
                      },
                mnesia:write(Registration),
                {ok, Receipt} = send_email(Email,
                                           uuid:uuid_to_string(Token, nodash)),
                pushdings:info("got receipt: ~p", [Receipt]),
                Email
        end,
    {atomic, Result} = mnesia:transaction(F),
    Result.

%% ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----

-spec exists(Email :: binary()) -> boolean().
exists(Email) ->
    length(mnesia:dirty_read(pushdings_registration, Email)) == 1.

%% ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----

-spec is_password_valid(Email :: binary(), Password :: binary()) -> boolean().
is_password_valid(Email, Password) ->
    HashedPassword = crypto:hash(sha256, Password),
    case mnesia:dirty_read(pushdings_registration, Email) of
        [#pushdings_registration{password = HashedPassword}] -> true;
        _                                                    -> false
    end.

%% -----------------------------------------------------------------------------

-spec install(Nodes :: list(atom())) -> ok.
install(Nodes) ->
    {atomic, ok} =
        mnesia:create_table(
          pushdings_registration,
          [{disc_copies, Nodes},
           {attributes, record_info(fields, pushdings_registration)}]),
    ok.

%% ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----

-spec tables() -> list(atom()).
tables() -> [pushdings_registration].

%% ------------------------------ < internal > ---------------------------------
-spec send_email(binary(), binary()) -> {ok, term()}
                                            | {error, term()}
                                            | {error, atom(), term()}.
send_email(Email, Token) ->
    MailFrom = pushdings:config(mail_from),
    MailOpts = pushdings:config(mail_opts, []),
    Message =
        io_lib:format(
          "Subject: Confirm your pushdings registration\r\n"
          "From: pushdings <~s>\r\n"
          "To: ~s\r\n\r\n"
          "To complete your registration at pushdings please fill in this"
          " token:\r\n\r\n"
          "\t~s\r\n\r\n"
          "Sincerely, your pushdings",
                [MailFrom, Email, Token]),
    pushdings:lift(
      gen_smtp_client:send_blocking(
        {MailFrom, [Email], Message},
        MailOpts)).
