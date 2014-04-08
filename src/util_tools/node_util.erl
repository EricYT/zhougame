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
	SNodeStr = atom_to_list(SNode),
	lists:foldl(fun(Node, Acc) when not Acc ->
						AppNodeStr = atom_to_list(Node),
						Index = string:str(SNodeStr, AppNodeStr),
						if
							Index >= 1 -> true;
							true -> false
						end;
				   (_, Acc) ->
						Acc
				end, false, get_appnodes(AppType)).

check_run_node(RRunNode) when is_list(RRunNode) ->
    CheckFun = fun(Node, Acc) when not Acc ->
                       Index = string:str(atom_to_list(node()), atom_to_list(Node)),
                       if
                           Index =/= 0 ->
                               true;
                           true ->
                               Acc
                       end;
                  (_Node, Acc) ->
                       Acc
               end,
    lists:foldl(CheckFun, false, RRunNode).


make_node_full_name(Node) when is_list(Node) ->
    Prefix = env:get(prefix, ""),
    Ip = env:get(ip, ""),
    erlang:list_to_atom(lists:concat([Prefix, Node, "@", Ip])).


get_appnodes(AppNodeType) ->
	system_option:get_node_option(AppNodeType).


get_all_nodes_by_appnodes(AppNodeTypes) ->
    AllNodes = global_node:get_all_nodes(),
    Res = lists:foldl(fun(SNode, AccInfo) ->
                        [lists:foldl(fun(Type, Acc) ->
                                             case check_snode_match(Type, SNode) of
                                                 true ->
                                                     [SNode|Acc];
                                                 false ->
                                                     Acc
                                             end
                                     end, [], AppNodeTypes)|AccInfo]
                end, [], AllNodes),
    lists:flatten(Res).


-spec split_node(Node::list()) -> {BaseName::atom(), Hostname::atom()}.
split_node(Node) when is_atom(Node) -> split_node(atom_to_list(Node));
split_node(Node) -> split_node_1(Node, []).

split_node_1([$@ | Cs], As) -> split_node_2(As, Cs);
split_node_1([C | Cs], As) -> split_node_1(Cs, [C|As]);
split_node_1([], As) -> split_node_2(As, "localhost").

split_node_2(As, Cs) -> {list_to_atom(lists:reverse(As)), list_to_atom(Cs)}.
  