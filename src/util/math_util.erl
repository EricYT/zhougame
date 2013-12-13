%% Author: Eric.yu
%% Created: 2013-9-7
%% Description: TODO: Add description to math_util
-module(math_util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
		 up_divide/2
		 ]).

%%
%% API Functions
%%
-spec up_divide(Dividend, Divisor) -> Res when
										 Dividend :: number(),
										 Divisor :: number(),
										 Res :: integer().
up_divide(Div, Divi) ->
	Temp = Div / Divi,
	if
		(Div div Divi) < Temp ->
			trunc(Temp)+1;
		true ->
			trunc(Temp)
	end.


%%
%% Local Functions
%%

