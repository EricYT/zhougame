%% Author: cb1224
%% Created: 2013-11-29
%% Description: TODO: Add description to str_util
-module(str_util).

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

sprintf(Format, Data) ->
	lists:flatten(io_lib:format(Format, Data)).

make_node(Prefix, SNode) when is_list(Prefix), is_list(SNode) ->
	list_to_atom(Prefix++"@"++SNode).


%%
%% Local Functions
%%

