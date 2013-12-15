%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-

{application, erlmysql_app,
[{description, "The webgame erlmysql_app application"},
 {vsn, "1.0"},
 {modules, [erlmysql_app,
            mysql_sup,
            mysql_manager]},
 {registered, [erlmysql_app]},
 {applications, [kernel, stdlib]},
 {mod, {erlmysql_app, []}}
 ]}.