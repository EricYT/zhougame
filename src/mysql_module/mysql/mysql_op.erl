%% Author: eric.yutao
%% Created: 2013-12-1
%% Description: TODO: mysql operation
-module(mysql_op).

%%
%% Include files
%%
-include("mysql.hrl").
-define(DEAULT_TIME, 5000).

%%
%% Exported Functions
%%
-export([]).
-compile(export_all).

%%
%% API Functions
%%
start() ->
    try
        ReadPoolId = mysql_util:get_pool_id_read(),
        WritePoolId = mysql_util:get_pool_id_write(),
        mysql:start(ReadPoolId, "localhost", "root", "123456", "webgame"),
        mysql:start(WritePoolId, "localhost", "root", "123456", "webgame")
    catch
        E:R ->
            debug:error("Error in ~p ~p~n", [?MODULE, {E, R}])
    end.



-spec read(Query) -> Result when
                                       Query :: string(),
                                       Result :: string().
read(Query) ->
    ReadPoolID = mysql_util:get_pool_id_read(),
    read(ReadPoolID, Query).


read(PoolId, Query) ->
    case mysql:fetch(PoolId, Query, ?DEAULT_TIME) of
        {data, #mysql_result{fieldinfo = FiledInfos, rows = Data}}=_Res ->
            io:format("mysql op ~p~n", [{_Res, Data}]),
            mysql_helper:unpack(FiledInfos, Data);
        {error, #mysql_result{error = Error}}=_Res ->
            erlang:binary_to_list(Error)
    end.



%%
%% Local Functions
%%

