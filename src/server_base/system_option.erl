%% Author: eric.yutao
%% Created: 2013-12-14
%% Description: TODO: A system option
-module(system_option).

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
get_node_option(Node) ->
	case lists:keyfind(Node, 1, nodes_option()) of
		false -> [];
		{_, NodeList} -> NodeList
	end.


%%
%% Local Functions
%%
nodes_option() ->
	[{gate, ['gate1', 'gate2', 'gate3']},
	 {map, ['map1', 'map2', 'map3']},
	 {gm, ['gm']},
	 {db, ['db']},
	 {line, ['line']},
	 {cache, ['cache']},
	 {auth, ['auth']}].


node_ets(App) ->
	NodeEts = [{map, all},
			   {line, ['map_db']}].
