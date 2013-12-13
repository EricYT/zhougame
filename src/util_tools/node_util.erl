%% Author: eric.yutao
%% Created: 2013-12-2
%% Description: TODO: Add description to node_util
-module(node_util).

%%
%% Include files
%%
-include("base.hrl").

%%
%% Exported Functions
%%
-export([]).
-compile(export_all).


%%
%% API Functions
%%
%% Get node sname by a full node name
get_node_sname(Node) ->
	StrNode = atom_to_list(Node),
	case string:tokens(StrNode, "@") of
		[NodeName, _Host] -> list_to_atom(NodeName);
		_ -> ?ERLNULL
	end.

%% Get the app name by node
get_node_name_by_node() ->
    NodeString = atom_to_list(get_node_sname(node())),
    Prefix = env:get(prefix, ""),
    NodeString--Prefix.

check_snode_match(AppType, SNode) ->
	true.


make_node_full_name(Node) when is_list(Node) ->
    Prefix = env:get(prefix, ""),
    Ip = env:get(ip, ""),
    erlang:list_to_atom(lists:concat([Prefix, Node, "@", Ip])).

%%
%% Local Functions
%%

