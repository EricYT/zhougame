%% Author: Eric.yu
%% Created: 2013-9-7
%% Description: TODO: Add description to cache_mod
-module(module_init_op).

%%
%% Include files
%%


%% --------------------------------------------------------------------
%% Macros
%% -------------------------------------------------------------------
-define(DIR_EBIN, "../ebin").
-define(BEHAVIOUR_MOD, module_op_mod).


-type file_name() :: string() | binary().
%%
%% Exported Functions
%%
-export([init/0, uninit/0]).

%%
%% API Functions
%%
-spec init() -> ok.
init() ->
	Modules = get_modules(),
	io:format("Modues ~p~n", [Modules]),
	init_module(Modules).


-spec uninit() -> ok.
uninit() ->
	Modules = get_modules(),
	io:format("Modues ~p~n", [Modules]),
	uninit_module(Modules).


-spec get_modules() -> Modules when
									   Modules :: [string(), ...].
get_modules() ->
	case filelib:wildcard(?DIR_EBIN++"/*.beam") of
		[] ->
			[];
		Beams ->
			BeamList = lists:foldl(fun(BeamFile, Acc) ->
											Beam = filename:basename(BeamFile, ".beam"),
											IsCMod = behaviour_util:is_behaviour(Beam, [?BEHAVIOUR_MOD]),
											if
												IsCMod ->
													[Beam]++Acc;
												true ->
													Acc
											end
									end, [], Beams),
			[erlang:list_to_atom(List) || List <- BeamList]
	end.


-spec init_module(Modules) -> ok when
										 Modules :: [atom(), ...].
init_module([]) ->
	ok;
init_module([H|Tail]) ->
	?BEHAVIOUR_MOD:init(H),
	init_module(Tail).


-spec uninit_module(Modules) -> ok when
									   Modules :: [atom(), ...].
uninit_module([]) ->
	ok;
uninit_module([H|Tail]) ->
	?BEHAVIOUR_MOD:uninit(H),
	uninit_module(Tail).


%%
%% Local Functions
%%
-spec get_module_name(File, Ext) -> Module when
										File :: file_name(),
										Ext :: file_name(),
										Module :: atom().
get_module_name(File, Ext) ->
	filename:basename(File, Ext).
