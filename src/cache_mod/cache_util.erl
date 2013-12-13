%% Author: Eric.yu
%% Created: 2013-9-8
%% Description: TODO: Add description to cache_util
-module(cache_util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
		 set/3,
		 del/2,
		 set_sync/2,
		 get_sync/2,
		 update_element/2,
		 make_cache_sup_name/1,
		 make_cache_processor_name/1
		 ]).

%%
%% API Functions
%%
-spec set(Ets, KeyValue, OverTime) -> true when
								   Ets :: atom(),
								   KeyValue :: tuple(),
								   OverTime :: non_neg_integer()|atom().
set(Ets, KeyValue, OverTime) ->
	KeyValueNew = erlang:append_element(KeyValue, OverTime),
	rpc:call(game_util:get_cache_node(), ets, insert, [Ets, KeyValueNew]).

-spec del(Ets, Key) -> true when
							  Ets :: atom(),
							  Key :: term().
del(Ets, Key) ->
	rpc:call(game_util:get_cache_node(), ets, delete, [Ets, Key]).

-spec update_element(Ets, KeyValues) -> boolean() when
													Ets :: atom(),
													KeyValues :: [tuple(), ...].
update_element(Ets, KeyValues) ->
	rpc:call(game_util:get_cache_node(), ets, update_element, [Ets, KeyValues]).


-spec set_sync(Ets, KeyValue) -> true when
										Ets :: atom(),
										KeyValue :: tuple().
set_sync(CacheProcessor, KeyValue) ->
	game_util:cast(game_util:get_cache_node(), CacheProcessor, {set, KeyValue}).

-spec get_sync(Ets, Key) -> list() when
									 Ets :: atom(),
									 Key :: term().
get_sync(CacheProcessor, Key) ->
	game_util:call(game_util:get_cache_node(), CacheProcessor, {get, Key}).



-spec make_cache_sup_name(CacheProcessor::atom()) -> CacheSup :: string().
make_cache_sup_name(ModName) when erlang:is_list(ModName) ->
	make_cache_sup_name(erlang:list_to_atom(ModName));
make_cache_sup_name(ModName) when erlang:is_atom(ModName) ->
	ModNameWithoutSpecialTag = list_util:lrec(erlang:atom_to_list(ModName), $$),
	erlang:list_to_atom(ModNameWithoutSpecialTag ++ "_sup");
make_cache_sup_name(_) ->
	error(badarg).
	
-spec make_cache_processor_name(CacheProcessor::atom()) -> CacheSup :: string().
make_cache_processor_name(ModName) when erlang:is_list(ModName) ->
	make_cache_processor_name(erlang:list_to_atom(ModName));
make_cache_processor_name(ModName) when erlang:is_atom(ModName) ->
	ModNameWithoutSpecialTag = list_util:lrec(erlang:atom_to_list(ModName), $$),
	erlang:list_to_atom(ModNameWithoutSpecialTag ++ "_processor");
make_cache_processor_name(_) ->
	error(badarg).
	
%%
%% Local Functions
%%

