%% Author: Eric.yu
%% Created: 2013-9-7
%% Description: TODO: Add description to cache_mod
-module(cache_module).

%%
%% Include files
%%


%% --------------------------------------------------------------------
%% Macros
%% -------------------------------------------------------------------
-define(DIR_EBIN, "../ebin").
-define(CACHE_MOD, cache_mod).


-type file_name() :: string() | binary().
%%
%% Exported Functions
%%
-export([init/0]).

%%
%% API Functions
%%
-spec init() -> ok.
init() ->
	CacheModules = get_cache_modules(),
	init_cache_module(CacheModules),
	load_cache_data(CacheModules).


-spec get_cache_modules() -> Modules when
									   Modules :: [string(), ...].
get_cache_modules() ->
	case filelib:wildcard(?DIR_EBIN++"/*.beam") of
		[] ->
			[];
		Beams ->
			BeamList = lists:foldl(fun(BeamFile, Acc) ->
											Beam = filename:basename(BeamFile, ".beam"),
											IsCacheMod = behaviour_util:is_behaviour(Beam, [?CACHE_MOD]),
											if
												IsCacheMod ->
													[Beam]++Acc;
												true ->
													Acc
											end
									end, [], Beams),
			[erlang:list_to_atom(List) || List <- BeamList]
	end.


-spec init_cache_module(Modules) -> ok when
										 Modules :: [atom(), ...].
init_cache_module([]) ->
	ok;
init_cache_module([H|Tail]) ->
	cache_mod:init(H),
	init_cache_module(Tail).


-spec load_cache_data(Modules) -> ok when
									   Modules :: [atom(), ...].
load_cache_data([]) ->
	ok;
load_cache_data([H|Tail]) ->
	cache_mod:load_data(H),
	load_cache_data(Tail).


%%
%% Local Functions
%%
-spec get_module_name(File, Ext) -> Module when
										File :: file_name(),
										Ext :: file_name(),
										Module :: atom().
get_module_name(File, Ext) ->
	filename:basename(File, Ext).
