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
    PoolId = mysql_util:get_pool_id_read(),
    Result = mysql:fetch(PoolId, Query),
    case Result of
        {updated, Info} ->
            Res = mysql:get_result_rows(Info),
            mysql_helper:unpacks(Res);
        {error, _Something} ->
            _Something
    end.

write(_Table, Query) ->
%%     PoolId = mysql_util:get_pool_id_write(),
    PoolId = mysql_util:get_pool_id_read(),
    Result = mysql:fetch(PoolId, Query),
    case Result of
        {updated, Info} ->
            Res = mysql:get_result_rows(Info),
            mysql_helper:unpacks(Res);
        {error, _Something} ->
            _Something
    end.

select(_Table, Query) ->
    PoolId = mysql_util:get_pool_id_read(),
    Result = mysql:fetch(PoolId, Query),
    case Result of
        {updated, Info} ->
            FiledInfos = mysql:get_result_field_info(Info),
            Rows = mysql:get_result_rows(Info),
            mysql_helper:unpacks(Rows);
        {error, _Something} ->
            _Something
    end.

transaction(_Table, Query) ->
    Fun = ok,
    PoolId = mysql_util:get_pool_id_read(),
    mysql:transaction(PoolId, Fun).



%%
%% Local Functions
%%

