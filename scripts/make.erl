#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname factorial debug

%% escript 文件中的第一行不能是erlang代码，否则将会被忽略
main([Options]) ->
	compile_all(Options);
main(_) ->
	compile_all().


compile_all(Options) ->
	case make:all(Options) of
		up_to_date ->
			halt(0);
		error ->
			halt(1)
	end.


compile_all() ->
	code:add_patha("../ebin"),
	case make:all() of
		up_to_date ->
			halt(0);
		error ->
			halt(1)
	end.


covert_args(Args) ->
	covert_args(Args, []).

covert_args([], Accargs) ->
	lists:reverse(Accargs);
covert_args([Args|Tail], AccArgs) when erlang:is_list(Args) ->
	covert_args(Tail, [erlang:list_to_tuple(Args)|AccArgs]);
covert_args([A|Tail], AccArgs) ->
	covert_args(Tail, [A|AccArgs]).
