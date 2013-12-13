%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-

{application, gate_app,
[{description, "The webgame gate application"},
 {vsn, "1.0"},
 {modules, [gate_app,
            gate_sup,
            gate_op]},
 {registered, [gate_sup]},
 {applications, [kernel, stdlib]},
 {mod, {gate_app, []}}
 ]}.