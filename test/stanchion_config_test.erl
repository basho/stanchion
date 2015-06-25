-module(stanchion_config_test).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

default_config_test() ->
    Config = cuttlefish_unit:generate_templated_config(schema_files(), [], context()),
    cuttlefish_unit:assert_config(Config, "stanchion.host", {"127.0.0.1", 8085}),
    cuttlefish_unit:assert_config(Config, "stanchion.riak_host", {"127.0.0.1", 8087}),
    cuttlefish_unit:assert_not_configured(Config, "stanchion.ssl"),
    cuttlefish_unit:assert_config(Config, "stanchion.admin_key", "admin-key"),
    cuttlefish_unit:assert_config(Config, "stanchion.admin_secret", "admin-secret"),
    cuttlefish_unit:assert_config(Config, "stanchion.auth_bypass", false),

    {ok, [ConsoleLog, ErrorLog]} = cuttlefish_unit:path(cuttlefish_variable:tokenize("lager.handlers"), Config),
    cuttlefish_unit:assert_config([ConsoleLog], "lager_file_backend", [{file, "./log/console.log"},
                                                                     {level, info},
                                                                     {size, 10485760},
                                                                     {date, "$D0"},
                                                                     {count, 5}]),
    cuttlefish_unit:assert_config([ErrorLog], "lager_file_backend", [{file, "./log/error.log"},
                                                                   {level, error},
                                                                   {size, 10485760},
                                                                   {date, "$D0"},
                                                                   {count, 5}]),
    ok.

ssl_config_test() ->
    Conf = [{["ssl", "certfile"], "path/certfile"},
            {["ssl", "keyfile"],  "path/keyfile"}],
    Config = cuttlefish_unit:generate_templated_config(schema_files(), Conf, context()),
    cuttlefish_unit:assert_config(Config, "stanchion.ssl", [{keyfile,  "path/keyfile"},
                                                          {certfile, "path/certfile"}]),
    ok.

lager_syslog_test() ->
    Conf = [{["log", "syslog"], on},
            {["log", "syslog", "ident"], "ident-test"},
            {["log", "syslog", "facility"], local7},
            {["log", "syslog", "level"], debug}
           ],
    Config = cuttlefish_unit:generate_templated_config(schema_files(), Conf, context()),
    cuttlefish_unit:assert_config(Config, "lager.handlers.lager_syslog_backend", ["ident-test", local7, debug]),
    ok.

lager_hander_test() ->
    Conf = [
            {["log", "console", "file"], "./log/consolefile.log"},
            {["log", "console", "level"], "debug"},
            {["log", "console", "size"], "1MB"},
            {["log", "console", "rotation"], "$D5"},
            {["log", "console", "rotation", "keep"], "10"},
            {["log", "error", "file"], "./log/errorfile.log"},
            {["log", "error", "size"], "1KB"},
            {["log", "error", "rotation"], "$D10"},
            {["log", "error", "rotation", "keep"], "20"}
           ],
    Config = cuttlefish_unit:generate_templated_config(schema_files(), Conf, context()),
    {ok, [ConsoleLog, ErrorLog]} = cuttlefish_unit:path(cuttlefish_variable:tokenize("lager.handlers"), Config),
    cuttlefish_unit:assert_config([ConsoleLog], "lager_file_backend", [{file, "./log/consolefile.log"},
                                                                       {level, debug},
                                                                       {size, 1048576},
                                                                       {date, "$D5"},
                                                                       {count, 10}]),
    cuttlefish_unit:assert_config([ErrorLog], "lager_file_backend", [{file, "./log/errorfile.log"},
                                                                     {level, error},
                                                                     {size, 1024},
                                                                     {date, "$D10"},
                                                                     {count, 20}]),

    CurrentConf1 = [{["log", "console", "rotation", "keep"], "current"}],
    Config1 = cuttlefish_unit:generate_templated_config(schema_files(), CurrentConf1, context()),
    {ok, [ConsoleLog1, _ErrorLog1]} = cuttlefish_unit:path(cuttlefish_variable:tokenize("lager.handlers"), Config1),
    cuttlefish_unit:assert_config([ConsoleLog1], "lager_file_backend.count", 0),

    CurrentConf2 = [{["log", "error", "rotation", "keep"], "current"}],
    Config2 = cuttlefish_unit:generate_templated_config(schema_files(), CurrentConf2, context()),
    {ok, [_ConsoleLog2, ErrorLog2]} = cuttlefish_unit:path(cuttlefish_variable:tokenize("lager.handlers"), Config2),
    cuttlefish_unit:assert_config([ErrorLog2], "lager_file_backend.count", 0),
    ok.

schema_files() ->
    ["../rel/files/stanchion.schema"].

context() ->
    {ok, Context} = file:consult("../rel/vars.config"),
    Context.
