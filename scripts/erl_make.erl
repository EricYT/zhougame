#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname factorial debug

%% escript 文件中的第一行不能是erlang代码，否则将会被忽略
main([Options]) ->
	compile_all(Options);
main(_) ->
	compile_all().


compile_all(Options) ->
	case mmake:all(get_cpu_cores(), Options) of
		up_to_date ->
			halt(0);
		error ->
			halt(1)
	end.


compile_all() ->
	%%TODO:增加一个版本控制的beam，以便于在编译的时候检查版本
	code:add_patha("../ebin"),
	case mmake:all(get_cpu_cores()) of
		up_to_date ->
			halt(0);
		error ->
			halt(1)
	end.

get_cpu_cores() ->
	case os:type() of
		{unix, _} ->
			CoreS = erlang:system_info(logical_processors) * 2;
		_ ->
			CoreS = erlang:system_info(logical_processors) - 1
	end,
	erlang:max(CoreS, 1).


covert_args(Args) ->
	covert_args(Args, []).

covert_args([], Accargs) ->
	lists:reverse(Accargs);
covert_args([Args|Tail], AccArgs) when erlang:is_list(Args) ->
	covert_args(Tail, [erlang:list_to_tuple(Args)|AccArgs]);
covert_args([A|Tail], AccArgs) ->
	covert_args(Tail, [A|AccArgs]).
