%% Author: Eric.yu
%% Created: 2013-10-7
%% Description: TODO: Add description to role_hook
-module(role_hook).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([handle/1]).

%%
%% API Functions
%%
handle({mod, Mod, Msg}) ->
	catch(Mod:handle(Msg));
handle(Msg) ->
	todo.


%%
%% Local Functions
%%

