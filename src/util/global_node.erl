%% Author: Administrator
%% Created: 2014-4-8
%% Description: TODO: Add description to global_node
-module(global_node).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([]).

-compile(export_all).
%%
%% API Functions
%%
get_all_nodes() ->
    %% If have a travel sever, include it
    [node()|nodes()].


%%
%% Local Functions
%%

