%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-

{application, role_app,
 [{description, "role application"},
  {vsn, "0.1.0"},
  {modules, [role_app,
             role_processor,
             role_processor_sup]},
  {registered, [role_app]},
  {applications, [kernel, stdlib]},
  {mod, {role_app, []}}
 }.
