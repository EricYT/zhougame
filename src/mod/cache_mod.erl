%% Author: Eric.yu
%% Created: 2013-9-8
%% Description: TODO: The behaviour for cache mod
-module(cache_mod).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([behaviour_info/1]).

-export([init/1, load_data/1]).

%%
%%-export([init_tables/0, load_data_from_db/0]).
%%

%%
%% API Functions
%%
behaviour_info(callbacks) ->
	[{init_tables, 0},
	 {load_data_from_db, 0}];
behaviour_info(_) -> undefined.


-spec init(Mod) -> ok when
						Mod :: atom().
init(Mod) when erlang:is_list(Mod) ->
	init(erlang:list_to_atom(Mod));
init(Mod) ->
	Mod:init_tables().


-spec load_data(Mod) -> ok when
							 Mod :: atom().
load_data(Mod) when erlang:is_list(Mod) ->
	load_data(erlang:list_to_atom(Mod));
load_data(Mod) ->
	Mod:load_data_from_db().
