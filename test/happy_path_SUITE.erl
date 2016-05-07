-module(happy_path_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1]).

-export([create_app/1]).

all() -> [create_app].

init_per_suite(Config) ->
    application:ensure_all_started(katt),
    application:ensure_all_started(gproc),
    gen_smtp_server:start(pushdings_smtp_mock),
    Config.

create_app(_Config) ->
    gproc_ps:subscribe(l, {pushdings_smtp_mock, data}),
    User = uuid:uuid_to_string(uuid:get_v4(), nodash),
    Email = User ++ "@example.com",
    Password = "topSecret",
    EmailBin = list_to_binary(Email),
    Authorization = base64:encode(Email ++ ":" ++ Password),

    {pass, _Filename1, _InParams1, OutParams1, _Trans1} =
        katt:run("../../test/register.apib", [{port, 8080},
                                              {email, Email},
                                              {password, Password},
                                              {authorization, Authorization}]),

    {[EmailBin], Msg} = await({pushdings_smtp_mock, data}, 5000),

    {match, [PosLen]} = re:run(Msg, "token:\r\n\r\n\t(.+)\r\n\r\n", [{capture, all_but_first}]),
    Token = binary:part(Msg, PosLen),

    {pass, _Filename2, _InParams2, _OutParams2, _Trans2} =
        katt:run("../../test/confirm_and_create.apib", [{token, Token}|OutParams1]),
    ok.


await(What, Timeout) ->
    receive
        {gproc_ps_event, What, Data} ->  Data;
        Unexpected -> ct:pal("unexpected: ~p", [Unexpected])
    after Timeout ->
            exit({timeout, {pushdings_smtp_mock, What}})
    end.
