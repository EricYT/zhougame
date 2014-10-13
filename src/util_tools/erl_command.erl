%% @author eric.yutao
%% @doc @todo Add description to erl_command.


-module(erl_command).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

-compile(export_all).



%% 
main(Cmd) ->
	Child = spawn(?MODULE, run, [Cmd, self()]),
	InputChild = spawn(?MODULE, inputloop, [Child, self()]),
	mainloop(InputChild).

run(Cmd, Main) ->
	Port = open_port({spawn, Cmd}, [exit_status, in, out, {line, 1024}]),
	run_loop(Port, Main).

run_loop(P, Main) ->
	receive
		{P, {data, {eol, Data}}} ->
			Exit = false,
			io:format("~s ~n", [Data]);
		{P, {data, Data}} ->
			Exit = false,
			io:format("~s ~n", [Data]);
		{P, {exit_status, Code}} ->
			Exit = true,
			io:format("exit ~p~n", [Code]);
		{input, Data} ->
			Exit = false,
			port_command(P, Data++"\n");
		{exit} ->
			port_close(P),
			Exit = true;
		Other ->
			Exit = false,
			io:format("rec ~p~n", [Other])
	end,
	if
		Exit ->
			Main ! {exit};
		true ->
			run_loop(P, Main)
	end.


inputloop(ProcPid, Main) ->
	{_, [Input]} = io:fread("Tell me what you want >> ", "~s"),
	if
		Input =:= "byebye" ->
			case erlang:is_process_alive(ProcPid) of
				true ->
					ProcPid ! {exit};
				false ->
					Main ! {exit}
			end,
			exit(normal);
		true ->
			ProcPid ! {input, Input},
			inputloop(ProcPid, Main)
	end.


mainloop(InputPid) ->
	receive
		{exit} ->
			Exit = true,
			erlang:exit(InputPid, kill);
		_Other ->
			Exit = false
	end,
	if
		Exit ->
			over;
		true ->
			mainloop(InputPid)
	end.

