%% Author: Eric.yu
%% Created: 2013-9-7
%% Description: TODO: Add description to who_is_undercover
-module(who_is_undercover).

-behaviour(module_op_mod).
%%
%% Include files
%%
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
	debug:info("Module ~p uninit~n", [?MODULE]),
	ok.


%%
%% Local Functions
%%
load_data_from_db() ->
	ok.

init_tables() ->
	ok.
