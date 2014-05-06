%% Author: eric.yutao
%% Created: 2013-12-2
%% Description: TODO: Add description to mysql_client
-module(mysql_client).

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
read(_Table, Query) ->
	%% fetch要增加一个server name
	ServerName = mysql_name_server:get_client(),
    PoolId = mysql_util:get_pool_id_read(),
    Info = mysql:fetch(ServerName, PoolId, Query),
    convert_data(Info).

insert(_Table, Query) ->
    ServerName = mysql_name_server:get_client(),
    PoolId = mysql_util:get_pool_id_write(),
    Result = mysql:fetch(ServerName, PoolId, Query),
    convert_data(Result).

select(_Table, Query) ->
    ServerName = mysql_name_server:get_client(),
    PoolId = mysql_util:get_pool_id_read(),
    Result = mysql:fetch(ServerName, PoolId, Query),
    convert_data(Result).

update(_Table, Query) ->
    ServerName = mysql_name_server:get_client(),
    PoolId = mysql_util:get_pool_id_write(),
    Result = mysql:fetch(ServerName, PoolId, Query),
    convert_data(Result).

transaction(_Table, Fun, Query) ->
    PoolId = mysql_util:get_pool_id_write(),
    ServerName = mysql_name_server:get_client(),
    mysql:transaction(ServerName, PoolId, Fun).



%%
%% Local Functions
%%
convert_data({data, Info}) ->
    io:format(">>>>>>>>>>>>>>>>> ~p~n", [{?MODULE, ?LINE, Info}]),
    mysql:get_result_rows(Info);
convert_data({updated, Info}) ->
    Nums = mysql:get_result_affected_rows(Info),
    io:format(">>>>>>>>>>>>>>>> update ~p~n", [{?MODULE, ?LINE, Info, Nums}]);
convert_data({error, Info}) ->
    Errors = mysql:get_result_reason(Info),
    io:format(">>>>>>>>>>>>>>>> error ~p~n", [{?MODULE, ?LINE, Info, Errors}]);
convert_data(_OtherInfo) -> 
    io:format(">>>>>>>>>>>>>>>>~p~n", [{?MODULE, ?LINE, _OtherInfo}]).


