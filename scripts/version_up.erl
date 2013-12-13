%% Author: Eric.yu
%% Created: 2013-10-6
%% Use for code hot update
-module(version_up).


-define(BEAM_VERSION_ETS, '$beams_version$').


-export([up_new/0, up_all/0, reload_all/0]).


-compile(export_all).

-spec init() -> ok.
init() ->
	%% 如果有新的beam加入时，只需要重新调用一次init即可。
	try
		ets:new(?BEAM_VERSION_ETS, [set, named_table, public])
	catch
		E:R -> io:format("version up init execption:~p ~p~n", [E, R])
	end,
	ets:insert(?BEAM_VERSION_ETS, get_beams_version()).


up_all() ->
	up_new().


up_new() ->
	BeamLists = get_beams_version(),
	NeedUpBeams = lists:filter(fun({Mod, Version}) ->
					OldVersion = get_old_version(Mod),
					if
						OldVersion =:= Version -> false;
						true -> true
					end
				   end, BeamLists),
	up_node(NeedUpBeams),
	ets:insert(?BEAM_VERSION_ETS, BeamLists).

reload_all() ->
	init(),
	AllModules = ets:tab2list(?BEAM_VERSION_ETS),
	up_node(AllModules).
	


%%
%% local fucntions
%%
get_beams_version() ->
	Beams = list_beam("./"),
	lists:map(fun(BeamFile) ->
				case beam_lib:version(BeamFile) of
					{ok, {Mod, Version}} -> {Mod, Version};
					_ -> {BeamFile, 0}
				end
			end, Beams).

list_beam(BeamDir) ->
	case file:list_dir(BeamDir) of
		{ok, Files} ->
			BeamFiles = lists:filter(fun(File) ->
							case lists:reverse(File) of
								"maeb."++_ -> true;
								_ -> false
							end
							end, Files),
			lists:map(fun(Beam) -> BeamDir ++ Beam end, BeamFiles);
		{error, Reason} -> io:format("List all beam file:~p~n", [Reason])
	end.


up_node([]) ->
	ok;
up_node([{Mod, _}|Tail]) ->
	c:l(Mod),
	io:format("----------------Version Up  Up Module:~p------------------~n", [Mod]),
	up_node(Tail).


get_old_version(Mod) ->
	case ets:lookup(?BEAM_VERSION_ETS, Mod) of
		[] -> [];
		[{Mod, Version}] -> Version
	end.
