%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-

{application, db_app,
[{description, "The webgame line application"},
 {vsn, "1.0"},
 {modules, [db_app,
            db_master]},
 {registered, [db_app]},
 {applications, [kernel, stdlib]},
 {mod, {db_app, []}}
 ]}.