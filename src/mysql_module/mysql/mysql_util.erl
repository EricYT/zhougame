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
-export([]).
-compile(export_all).

%%
%% API Functions
%%
-spec get_pool_id_read() -> ReadPoolId when
                                         ReadPoolId :: atom().
get_pool_id_read() ->
    read.


-spec get_pool_id_write() -> ReadPoolId when
                                         ReadPoolId :: atom().
get_pool_id_write() ->
    write.


%%
%% Local Functions
%%

