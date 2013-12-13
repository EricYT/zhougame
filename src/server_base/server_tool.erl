%% Author: cb1224
%% Created: 2013-11-29
%% Description: TODO: Add description to server_tool
-module(server_tool).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([run/0, print_tool/0]).

-compile(export_all).

%%
%% API Functions
%%
run() ->
    applicationex:force_start(),
    NodeName = node_util:get_node_name_by_node(),
    AppName = list_to_atom(NodeName++"_app"),
    debug:info("*************Server tool ~p~n", [{?MODULE, node(), AppName}]),
    applicationex:start(AppName).

print_tool() ->
	ok.

test() ->
    io:format("Error1111 Test ~p~n", [?MODULE]).


%%
%% Local Functions
%%

