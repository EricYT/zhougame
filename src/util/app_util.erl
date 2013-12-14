%% Author: eric.yutao
%% Created: 2013-12-14
%% Description: TODO: The tool module for application
-module(app_util).

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
get_argument(Input) when is_atom(Input) ->
	case init:get_argument(Input) of
		error -> [];
		{ok, [ArgString]} -> [list_to_atom(Value)||Value<-ArgString]
	end;
get_argument(Input) when is_list(Input) ->
	get_argument(list_to_atom(Input));
get_argument(_) ->
	[].


%%
%% Local Functions
%%

