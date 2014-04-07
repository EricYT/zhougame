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
%% 	AffectedNum = mysql:get_result_affected_rows(Info),
%% 	{ok, AffectedNum}.
%%     case Result of
%%         {data, Info} ->
%%             Res = mysql:get_result_rows(Info),
%%             mysql_helper:unpacks(Res);
%%         {error, _Something} ->
%%             _Something
%%     end.

write(_Table, Query) ->
    ServerName = mysql_name_server:get_client(),
    PoolId = mysql_util:get_pool_id_write(),
    Result = mysql:fetch(ServerName, PoolId, Query),
    convert_data(Result).
%%     case Result of
%%         {updated, Info} ->
%%             Res = mysql:get_result_rows(Info),
%%             mysql_helper:unpacks(Res);
%%         {error, _Something} ->
%%             _Something
%%     end.

select(_Table, Query) ->
    ServerName = mysql_name_server:get_client(),
    PoolId = mysql_util:get_pool_id_read(),
    Result = mysql:fetch(ServerName, PoolId, Query),
    convert_data(Result).
%%     case Result of
%%         {updated, Info} ->
%%             FiledInfos = mysql:get_result_field_info(Info),
%%             Rows = mysql:get_result_rows(Info),
%%             mysql_helper:unpacks(Rows);
%%         {error, _Something} ->
%%             _Something
%%     end.

transaction(_Table, Query) ->
    Fun = ok,
    PoolId = mysql_util:get_pool_id_read(),
    mysql:transaction(PoolId, Fun).



%%
%% Local Functions
%%
convert_data({data, Info}) ->
    mysql:get_result_rows(Info);
convert_data({updated, Info}) ->
    Nums = mysql:get_result_affected_rows(Info),
    io:format(">>>>>>>>>>>>>>>> update ~p~n", [{Info, Nums}]);
convert_data({error, Info}) ->
    Errors = mysql:get_result_reason(Info),
    io:format(">>>>>>>>>>>>>>>> error ~p~n", [{Info, Errors}]);
convert_data(_OtherInfo) ->
    io:format(">>>>>>>>>>>>>>>>~p~n", [_OtherInfo]).


