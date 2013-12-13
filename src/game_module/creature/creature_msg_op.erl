%% Author: Administrator
%% Created: 2013-10-7
%% Description: TODO: The module operate the msg.
-module(creature_msg_op).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([make_mod_msg/2]).

%%
%% API Functions
%%
send_to_one_proc(RoleId, Msg) ->
	todo.


make_mod_msg(Module, Msg) ->
	{mod, Module, Msg}.


%%
%% Local Functions
%%

