%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-

{application, map_app,
[{description, "The webgame map application"},
 {vsn, "1.0"},
 {modules, [map_app,
            map_sup,
            map_manager]},
 {registered, [map_app]},
 {applications, [kernel, stdlib]},
 {mod, {map_app, []}}
 ]}.