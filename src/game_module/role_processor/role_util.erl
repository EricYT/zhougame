%% Author: Eric.yu
%% Created: 2013-10-7
%% Description: TODO: Add description to role_util
-module(role_util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([make_role_processor_sup_name/1, make_role_processor_name/1]).

%%
%% API Functions
%%
make_role_processor_sup_name(RoleId) when erlang:is_integer(RoleId) ->
	Name = "role_processor_sup_" ++ erlang:integer_to_list(RoleId),
	erlang:list_to_atom(Name).


make_role_processor_name(RoleId) when erlang:is_integer(RoleId) ->
	Name = "role_processor_" ++ erlang:integer_to_list(RoleId),
	erlang:list_to_atom(Name).


%%
%% Local Functions
%%

