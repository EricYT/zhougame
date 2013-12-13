%% Author: Eric.yu
%% Created: 2013-9-7
%% Description: TODO: The game opreator
-module(module_op_mod).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([behaviour_info/1]).

-export([init/1, uninit/1]).

%% use this behaviour export these functions
%% -export([
%% 		 init/1,
%% 		 handle/1,
%% 		 uninit/0
%% 		 ]).

%%
%% API Functions
%%
behaviour_info(callbacks) ->
	[{init, 0},
	 {handle, 1},
	 {uninit, 0}];
behaviour_info(_) ->
	undefined.


%%
%% Local Functions
%%

-spec init(Mod) -> ok when
						Mod :: atom().
init(Mod) when erlang:is_list(Mod) ->
	init(erlang:list_to_atom(Mod));
init(Mod) ->
	Mod:init().


-spec uninit(Mod) -> ok when
							 Mod :: atom().
uninit(Mod) when erlang:is_list(Mod) ->
	uninit(erlang:list_to_atom(Mod));
uninit(Mod) ->
	Mod:uninit().