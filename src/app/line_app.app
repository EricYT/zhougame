%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-

{application, line_app,
[{description, "The webgame line application"},
 {vsn, "1.0"},
 {modules, [line_app,
            line_sup,
            line_manager]},
 {registered, [line_app]},
 {applications, [kernel, stdlib]},
 {mod, {line_app, []}}
 ]}.