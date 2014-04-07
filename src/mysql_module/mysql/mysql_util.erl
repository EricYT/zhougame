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
		 get_config/1,
         get_pool_id_read/0,
         get_pool_id_write/0,
         get_client_size/0,
         get_app_run_node/0,
         get_r_pool_size/0,
         get_w_pool_size/0,
         get_l_pool_size/0,
         get_r_conf/0,
         get_w_conf/0,
         get_l_conf/0
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


-spec get_app_run_node() -> RunNodes when
                                        RunNodes :: [Node, ...],
                                        Node :: atom().
get_app_run_node() ->
    get_config(app_run_node).


-spec get_client_size() -> ClientSize when
                                        ClientSize :: non_neg_integer().
get_client_size() ->
    get_config(client_size).


-spec get_r_pool_size() -> PoolSize when
                                      PoolSize :: integer().
get_r_pool_size() ->
    get_config(r_pool_size).


-spec get_w_pool_size() -> PoolSize when
                                      PoolSize :: integer().
get_w_pool_size() ->
    get_config(w_pool_size).


-spec get_l_pool_size() -> PoolSize when
                                      PoolSize :: integer().
get_l_pool_size() ->
    get_config(log_pool_size).

%%[PoolId, WHost, WPort, WUser, WPwd, WDB, WEncoding, WRunNode]
-spec get_w_conf() -> Config when
                               Config :: list().
get_w_conf() ->
    PoolId      = get_pool_id_write(),
    WHost       = get_config(w_host),
    WPort       = get_config(w_port),
    WUser       = get_config(w_user),
    WPwd        = get_config(w_pwd),
    WDB         = get_config(w_database),
    WEncoding   = get_config(w_encoding),
    WRunNode    = get_config(r_runnode),
    [PoolId, WHost, WPort, WUser, WPwd, WDB, WEncoding, WRunNode].


%%[LPoolId, LHost, LPort, LUser, LPwd, LDB, LEncoding, LRunNode]
-spec get_l_conf() -> Config when
                               Config :: list().
get_l_conf() ->
    LPoolId     = get_pool_id_write(),
    LHost       = get_config(log_host),
    LPort       = get_config(log_port),
    LUser       = get_config(log_user),
    LPwd        = get_config(l_pwd),
    LDB         = get_config(log_database),
    LEncoding   = get_config(log_encoding),
    LRunNode    = get_config(log_runnode),
    [LPoolId, LHost, LPort, LUser, LPwd, LDB, LEncoding, LRunNode].


%%[ReadPoolId, RHost, RPort, RUser, RPwd, RDB, REncoding, RRunNode]
-spec get_r_conf() -> Config when
                               Config :: list().
get_r_conf() ->
    RPoolId     = get_pool_id_read(),
    RHost       = get_config(r_host),
    RPort       = get_config(r_port),
    RUser       = get_config(r_user),
    RPwd        = get_config(r_pwd),
    RDB         = get_config(r_database),
    REncoding   = get_config(r_encoding),
    RRunNode    = get_config(r_runnode),
    [RPoolId, RHost, RPort, RUser, RPwd, RDB, REncoding, RRunNode].


%%
%%@doc
%%@date:2014-1-4
%%

%%
%% Local Functions
%%
-spec get_config(Key::atom()) -> [] | Value when
											  Value :: list().
get_config(Key) ->
	MysqlConfig = env:get(mysql, []),
	case lists:keyfind(Key, 1, MysqlConfig) of
		{_, Value} -> Value;
		false -> []
	end.
