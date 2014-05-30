%% Author: cb1224
%% Created: 2013-11-28
%% Description: TODO: Add description to run_option
-module(run_option).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([get_options/1]).
-compile(export_all).

%%
%% API Functions
%%
-spec get_options(FileName) -> Options|Error when
											   FileName :: string(),
											   Options :: [string(),...],
											   Error :: atom().
get_options(OptionFile) ->
	case file:consult(OptionFile) of
		{ok, [Options]} ->
			Options;
		{error, Reason} ->
			io:format("~p ~p~n", [?MODULE, Reason]),
			[]
	end.

get_opt_nodes(Options) ->
	case lists:keyfind(nodes, 1, Options) of
		false -> undefined;
		{_, NodesOption} -> NodesOption
	end.

get_opt_prefix(Options) ->
	case lists:keyfind(prefix, 1, Options) of
		false -> "";
		{_, Prefix} -> Prefix
	end.

get_opt_common_option(Options) ->
	case lists:keyfind(common_option, 1, Options) of
		false -> undefined;
		{_, CommonOption} -> CommonOption
	end.

get_opt_beam_dir(Options) ->
	case lists:keyfind(beam_dir, 1, Options) of
		false -> undefined;
		{_, BeamDir} -> BeamDir
	end.

get_opt_db_dir(Options) ->
	case lists:keyfind(db_dir, 1, Options) of
		false -> undefined;
		{_, DbBir} -> DbBir
	end.

get_opt_cache_node(Options) ->
	case lists:keyfind(cache_node, 1, Options) of
		false -> undefined;
		{_, CacheNode} -> CacheNode
	end.

get_opt_tools_node(Options) ->
	case lists:keyfind(tool_nodes, 1, Options) of
		false -> undefined;
		{_, CacheNode} -> CacheNode
	end.


%%
%% Local Functions
%%
