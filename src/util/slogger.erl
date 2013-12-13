%% Author: Eric.yu
%% Created: 2013-9-7
%% Description: TODO: Add description to slogger
-module(slogger).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
		 msg/1,
		 msg/2
		 ]).


%%
%% Type
-type format() :: atom() | string() | binary().


%%
%% API Functions
%%
-spec msg(Format) -> ok when
								Format :: format().
msg(Format) ->
	io:format(Format),
	error_logger:error_msg(Format).


-spec msg(Format, Data) -> ok when
								Format :: format(),
								Data :: [term()].
msg(Format, Data) ->
	io:format(Format, Data),
	error_logger:error_msg(Format, Data).


%% -spec error(Format) -> ok when
%% 							Format :: format().
%% error(Format) ->
%% 	io:format(Format),
%% 	error_logger:info_msg(Format).


%%
%% Local Functions
%%

