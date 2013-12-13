%% Author: Eric.yu
%% Created: 2013-9-8
%% Description: TODO: Add description to template_cache_mod
-module(template_cache_mod).

-behaviour(cache_mod).

-define(CACHE_TEMPLATE_ETS, '$cache_template_ets').
-define(CACHE_TEMPLATE_TIME_ETS, '$cache_template_time_ets').
-define(CACHE_TEMPLATE_PROCESSOR, '$cache_template_processor').
-define(CACHE_TEMPLATE_OVER_TIME, infinity).
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([]).

-export([init_tables/0, load_data_from_db/0]).

-compile(export_all).
%%
%% API Functions
%%
-spec init_tables() -> ok.
init_tables() ->
	ets:new(?CACHE_TEMPLATE_ETS, [set, named_table, {keypos, 1}]),
	ets:new(?CACHE_TEMPLATE_TIME_ETS, [set, named_table, {keypos, 1}]),
	cache_processor_sup:start_link(?CACHE_TEMPLATE_PROCESSOR, ?CACHE_TEMPLATE_ETS, ?CACHE_TEMPLATE_OVER_TIME),
	ok.


-spec load_data_from_db() -> ok.
load_data_from_db() ->
	ok.


%%
%% Local Functions
%%
-spec set_data_into_cache(KeyValue) -> no_return() when
													 KeyValue :: tuple().
set_data_into_cache(KeyValue) ->
	cache_util:set(?CACHE_TEMPLATE_ETS, KeyValue, ?CACHE_TEMPLATE_OVER_TIME).

-spec del_data_from_cache(Key) -> true when
										 Key :: term().
del_data_from_cache(Key) ->
	cache_util:del(?CACHE_TEMPLATE_ETS, Key).

-spec update_data_in_cache(KeyValues) -> boolean() when
													KeyValues :: [tuple(), ...].
update_data_in_cache(KeyValueLists) when erlang:is_list(KeyValueLists) ->
	cache_util:update_element(KeyValueLists).
