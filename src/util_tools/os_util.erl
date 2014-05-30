%% Author: cb1224
%% Created: 2013-11-29
%% Description: TODO: Add description to os_util
-module(os_util).

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
-spec match_ip(IP) -> boolean() when
								  IP :: {A, B, C, D},
								  A :: non_neg_integer(),
								  B :: non_neg_integer(),
								  C :: non_neg_integer(),
								  D :: non_neg_integer().
match_ip(GiveIp) when erlang:is_list(GiveIp) ->
	case inet:getif() of
		{ok, IFs} ->
			check_is_any_match(IFs, GiveIp, false);
		_ -> false
	end;
match_ip(_) -> false.

check_is_any_match([], _, Acc) ->
	Acc;
check_is_any_match([{{A, B, C, D}, _, _}|Tail], MatchIp, Acc) when not Acc ->
	AI = erlang:integer_to_list(A),
	BI = erlang:integer_to_list(B),
	CI = erlang:integer_to_list(C),
	DI = erlang:integer_to_list(D),
	IsMatch = string:join([AI, BI, CI, DI], ".") =:= MatchIp,
	if
		IsMatch ->
			true;
		true ->
			check_is_any_match(Tail, MatchIp, Acc)
	end;
check_is_any_match([_|Tail], MatchIp, Acc) ->
	check_is_any_match(Tail, MatchIp, Acc).


-spec linux_run(ComLine, Flag) ->  ok when
										ComLine :: string(),
										Flag :: wait | atom().
linux_run(CmdLine, Flag) ->
	case os:type() of
		{win32, nt} ->
			io:format("Windows does not run:~p~n", [CmdLine]);
		_ ->
			case Flag of
				wait -> wait_exe(CmdLine);
				_ -> run_exe(CmdLine)
			end
	end.

win_run(CmdLine, Flag) ->
	case os:type() of
		{win32, nt} ->
			case Flag of
				wait -> wait_exe(CmdLine);
				_ -> run_exe(CmdLine)
			end;
		_ ->
			io:format("linux does not run:~p~n", [CmdLine])
	end.


run_erl(Hiden, Name, Host, MnesiaDir, SmpEnable, Wait, Option) ->
	CommandLine = get_erl_cmd(Hiden, Name, Host, MnesiaDir, SmpEnable, Wait, Option),
	io:format("Command ~p~n", [CommandLine]),
	case Wait of
		wait ->
			wait_exe(CommandLine);
		nowait ->
			run_exe(CommandLine)
	end.


wait_exe(CmdLine) ->
	wait_exe(CmdLine, prompt).

wait_exe(CmdLine, noprompt) ->
	os:cmd(CmdLine);
wait_exe(CmdLine, prompt) ->
%% 	debug:info("~s~n", [CmdLine]),
	os:cmd(CmdLine).

run_exe(CmdLine) ->
	run_exe(CmdLine, prompt).

run_exe(CmdLine, noprompt) ->
	cmd_ansync(CmdLine);
run_exe(CmdLine, _) ->
	%%debug:info("~s~n", [CmdLine]),
	cmd_ansync(CmdLine).


%%
%% Local Functions
%%

cmd_ansync(Cmd) ->
	CurPid = self(),
	Fun = fun() -> do_cmd_ansync(Cmd, CurPid) end,
	proc_lib:spawn(Fun),
	receive
		ok -> ok;
		_ -> error
	end.


do_cmd_ansync(Cmd, MonitorPid) ->
	validate(Cmd),
	case os:type() of
		{unix, _} ->
			unix_cmd(Cmd, MonitorPid);
		{win32, WType} ->
			Command = case {os:getenv("COMSPEC"), WType} of
						  {false, windows} -> lists:concat(["command.com /c ", Cmd]);
						  {false, _} -> lists:concat(["cmd /c ", Cmd]);
						  {CSpec, _} -> lists:concat([CSpec, " /c ", Cmd])
					  end,
			Port = open_port({spawn, Command}, [stream, in, eof, hide]),
			MonitorPid ! ok,
			get_data(Port, []);
		vxworks ->
			Command = lists:concat(["sh -c '", Cmd, "'"]),
			Port = open_port({spawn, Command}, [stream, in, eof]),
			MonitorPid ! ok,
			get_data(Port, [])
	end.


get_data(Port, Sofar) ->
	receive
		{Port, {data, Bytes}} ->
			get_data(Port, [Sofar|Bytes]);
		{Port, eof} ->
			Port ! {self(), close},
			receive
				{Port, closed} ->
					true
			end,
			receive
				{'EXIT', Port, _} ->
					ok
			after 1 ->			%% force context switch
					ok
			end,
			lists:flatten(Sofar)
	end.


unix_cmd(Cmd, MonitorPid) ->
	Tag = erlang:make_ref(),
	{Pid, Mref} = erlang:spawn_monitor(
					fun() ->
							process_flag(trap_exit, true),
							Port = start_port(),
							erlang:port_command(Port, mk_cmd(Cmd)),
							MonitorPid ! ok,
							exit({Tag, unix_get_data(Port)})
					end),
	receive
		{'DOWN', Mref, _, Pid, {Tag, Result}} ->
			Result;
		{'DOWN', Mref, _, Pid, Reason} ->
			exit(Reason)
	end.


-define(PORT_CREATOR_NAME, os_cmd_port_creator).
-define(SHELL, "/bin/sh -s unix:cmd 2>&1").

start_port() ->
	Ref = make_ref(),
	Request = {Ref, self()},
	{Pid, Mon} = case whereis(?PORT_CREATOR_NAME) of
					 undefined ->
						 spawn_monitor(fun() ->
											   start_port_srv(Request)
									   end);
					 P ->
						 P ! Request,
						 M = erlang:monitor(process, P),
						 {P, M}
				 end,
	receive
		{Ref, Port} when is_port(Port) ->
			erlang:demonitor(Mon, [flush]),
			Port;
		{Ref, Error} ->
			erlang:demonitor(Mon, [flush]),
			exit(Error);
		{'DOWN', Mon, process, Pid, _Reason} ->
			start_port()
	end.

start_port_srv(Request) ->
	%% We don't want a group leader of some random application.Use
	%% kernel_sup's group leader.
	{group_leader, GL} = process_info(whereis(kernel_sup),
									   group_leader),
	true = group_leader(GL, self()),
	process_flag(trap_exit, true),
	StayAlive = try register(?PORT_CREATOR_NAME, self())
				catch
					error:_ -> false
				end,
	start_port_srv_handle(Request),
	case StayAlive of
		true -> start_port_srv_loop();
		false -> exiting
	end.

start_port_srv_handle({Ref, Client}) ->
	Reply = try open_port({spawn, ?SHELL}, [stream]) of
				Port when is_port(Port) ->
					(catch port_connect(Port, Client)),
					unlink(Port),
					Port
			catch
				error:Reason ->
					{Reason, erlang:get_stacktrace()}
			end,
	Client ! {Ref, Reply}.

start_port_srv_loop() ->
	receive
		{Ref, Client} = Request when is_reference(Ref),
									 is_pid(Client) ->
			start_port_srv_handle(Request);
		_Junk ->
			ignore
	end,
	start_port_srv_loop().


unix_get_data(Port) ->
	unix_get_data(Port, []).

unix_get_data(Port, Sofar) ->
	receive
		{Port, {data, Bytes}} ->
			case eot(Bytes) of
				{done, Last} ->
					lists:flatten([Sofar|Last]);
				more ->
					unix_get_data(Port, [Sofar|Bytes])
			end;
		{'EXIT', Port, _} ->
			lists:flatten(Sofar)
	end.



validate(Atom) when erlang:is_atom(Atom) ->
	ok;
validate(List) when erlang:is_list(List) ->
	validate1(List).

validate1([C|Rest]) when erlang:is_integer(C), 0 =< C, C < 256 ->
	validate(Rest);
validate1([List|Rest]) when erlang:is_list(List) ->
	validate1(List),
	validate1(Rest);
validate1([]) ->
	ok.

mk_cmd(Cmd) when is_atom(Cmd) ->
	mk_cmd(atom_to_list(Cmd));
mk_cmd(Cmd) ->
	%% We insert a new line after the command, in case the command
	%% contains a comment character
	io_lib:format("(~s\n) </dev/null; echo \"\^D\"\n", [Cmd]).


eot(Bs) ->
	eot(Bs, []).

eot([4|_Bs], As) ->
	{done, lists:reverse(As)};
eot([B|Bs], As) ->
	eot(Bs, [B|As]);
eot([], _As) ->
	more.


get_erl_cmd(Hiden, Name, Host, MnesiaDir, SmpEnable, Wait, Option) ->
	HidenOption = case Hiden of
					  hiden -> " -noshell -noinput ";
					  _ ->""
				  end,
	NameOption = case Name of
					 [] -> "";
					 _ -> str_util:sprintf(" -name ~s@~s ", [Name, Host])
				 end,
	DBOption = case MnesiaDir of
				   [] -> "";
				   _ -> str_util:sprintf(" -mnesia dir '\"~s\"' ", [MnesiaDir])
			   end,
	
	SMPOption = case SmpEnable of
					smp -> "";
					nosmp ->
						case os:type() of
							{win32, nt} -> " ";
							_ -> " -smp disable "
						end
				end,
	ExeCmd = case os:type() of
				 {win32, nt} ->
					 case Wait of
						 wait ->
						 	"start erl.exe ";
						 nowait ->
							 case Hiden of
								 hiden -> "erl.exe";
								 normal -> "start cmd.exe /k erl.exe ";
								 gui -> "werl.exe"
							 end
					 end;
				 _ -> "erl +P 100000 +K true"
			 end,
	
	TailOption = case os:type() of
					 {win32, nt} -> "";
					 _ ->
						 case Wait of
							 wait -> "";
							 nowait -> " > /dev/null 2>&1 &"
						 end
				 end,
	lists:append([ExeCmd, HidenOption, NameOption, DBOption, SMPOption, Option, TailOption]).











