%% Author: Administrator
%% Created: 2013-11-24
%% Description: TODO: Add description to compile_lua_files
-module(compile_lua_files).

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
traverse_dir(Dir) when erlang:is_atom(Dir) ->
	traverse_dir(erlang:atom_to_list(Dir));
traverse_dir(Dir) ->
	case filelib:is_dir(Dir) of
		true ->
			case file:list_dir(Dir) of
				{ok, Comments} ->
					{Files, Dirs} = filter_files_and_directories(Comments, [], [], Dir),
					DirFiles = [begin
									traverse_dir(DirValue)
								end||DirValue <- Dirs],
					lists:flatten(Files++DirFiles);
				Error ->
					Error
			end;
		_ ->
			nothing
	end.


filter_files_and_directories([], AccFiles, AccDirs, _) ->
	{AccFiles, AccDirs};
filter_files_and_directories([".svn"|Tail], AccFiles, AccDirs, Dir) ->
	filter_files_and_directories(Tail, AccFiles, AccDirs, Dir);
filter_files_and_directories([Object|Tail], AccFiles, AccDirs, Dir) ->
	FullName = Dir++"/"++Object,
	case filelib:is_dir(FullName) of
		true ->
			filter_files_and_directories(Tail, AccFiles, [list_to_atom(FullName)|AccDirs], Dir);
		_ ->
			IsErl = lists:suffix(".erl", Object),
			if
				IsErl ->
					filter_files_and_directories(Tail, [list_to_atom(FullName)|AccFiles], AccDirs, Dir);
				true ->
					filter_files_and_directories(Tail, AccFiles, AccDirs, Dir)
			end
	end.


%%
%% Local Functions
%%

