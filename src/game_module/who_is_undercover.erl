%% Author: Eric.yu
%% Created: 2013-9-7
%% Description: TODO: Add description to who_is_undercover
-module(who_is_undercover).

-behaviour(module_op_mod).
%%
%% Include files
%%
-include("model_han_grave_db.hrl").

%%
%% Exported Functions
%%
-export([]).

-export([init/0, handle/1, uninit/0]).


%%
%% API Functions
%%
-spec init() -> ok.
init() ->
	debug:info("Module ~p init~n", [?MODULE]),
	ok.


-spec handle(Msg) -> ok when
								   Msg :: record().
handle(_Other) ->
	debug:info("Error Msg to handle ~p~n", [_Other]).


-spec uninit() -> ok.
uninit() ->
    DbInfo = #role_han_grave_db{roleid              = 321,
                                count               = 0,
                                last_call_quality   = 1,
                                monster_info        = [{3,4}],
                                update_time         = now()},
    model_role_han_grave_db:write(DbInfo),
	ok.


%%
%% Local Functions
%%
load_data_from_db() ->
	ok.

init_tables() ->
	ok.
