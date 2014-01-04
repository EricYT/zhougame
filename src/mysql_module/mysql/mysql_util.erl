%% Author: Administrator
%% Created: 2013-12-1
%% Description: TODO: Add description to mysql_util
-module(mysql_util).

%%
%% Include files
%%



%%
%% Exported Functions
%%
-export([
		 
		 ]).

%%
%% API Functions
%%
-compile({inline, [get_pool_id_read/0]}).
-spec get_pool_id_read() -> ReadPoolId when
                                         ReadPoolId :: atom().
get_pool_id_read() ->
    read.

-compile({inline, [get_pool_id_write/0]}).
-spec get_pool_id_write() -> ReadPoolId when
                                         ReadPoolId :: atom().
get_pool_id_write() ->
    write.


%%
%%@doc
%%@date:2014-1-4
%%

%%
%% Local Functions
%%
-spec get_config(Key::atom()) -> [] | Value when
											  Key :: 
get_confige(Key) ->
	MysqlConfig = env:get(mysql, []),
	case lists:keyfind(Key, 1, MysqlConfig) of
		{_, Value} -> Value;
		false -> []
	end.
