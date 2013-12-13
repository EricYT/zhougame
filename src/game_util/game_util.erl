%% Author: Eric.yu
%% Created: 2013-9-8
%% Description: TODO: The game util
-module(game_util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
		 send/3,
		 call/3
		 ]).

%%
%% API Functions
%%
-spec send(Node, Processor, Msg) -> Res when
										Node :: node(),
										Processor :: atom(),
										Msg :: [term(), ...],
										Res :: term().
send(Node, Processor, Msg) when Node =:= node() ->
	Processor ! Msg;
send(Node, Processor, Msg) ->
	Nodes = erlang:nodes() ++ erlang:nodes(hidden),
	case lists:member(Node, Nodes) of
		true ->
			{Processor, Node} ! Msg;
		false ->
			{error, non_node}
	end.

-spec call(Node, Processor, Msg) -> Res when
										Node :: node(),
										Processor :: atom(),
										Msg :: [term(), ...],
										Res :: term().
call(Node, Processor, Msg) when Node =:= node() ->
	catch(gen_server:call(Processor, Msg));
call(Node, Processor, Msg) ->
	Nodes = erlang:nodes() ++ erlang:nodes(hidden),
	case lists:member(Node, Nodes) of
		true ->
			catch(gen_server:call({Processor, Node}, Msg));
		false ->
			{error, non_node}
	end.


%%
%% Local Functions
%%

