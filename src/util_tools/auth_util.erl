%% Author: cb1224
%% Created: 2013-8-10
%% Description: TODO: Add description to auth_util
-module(auth_util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([]).

-compile([export_all]).

%%
%% API Functions
%%
-spec escape_uri(Binary) -> Binary1 when
									  Binary :: binary(),
									  Binary1 :: binary().
escape_uri(S) when erlang:is_list(S) ->
	escape_uri(unicode:characters_to_binary(S));
escape_uri(<<C:8, Cs/binary>>) when C >= $a, C =< $z ->
	[C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C >= $A, C =< $Z ->
	[C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C >= $0, C =< $9 ->
	[C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C =:= $. ->
	[C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C =:= $- ->
	[C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) when C =:= $_ ->
	[C] ++ escape_uri(Cs);
escape_uri(<<C:8, Cs/binary>>) ->
	escape_byte(C) ++ escape_uri(Cs);
escape_uri(<<>>) ->
	"".


%% Local Functions
%%
escape_byte(_) ->
	ok.