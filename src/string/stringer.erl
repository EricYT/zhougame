%% Author: eric.yutao
%% Created: 2013-12-15
%% Description: TODO: A file do some funning things.
-module(stringer).

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

-spec stringer(V) -> list() when
                                V :: atom(),
                                V :: list().
stringer(V) when is_list(V) ->
    fun() -> stringer(V, nop) end;
stringer(V) when is_atom(V) ->
    fun() -> stringer(V, nop) end;
stringer(_V) ->
    fun() -> not_implemented end.

-spec stringer(V, Nop) -> list() when
                                   V :: atom()|list(),
                                   Nop :: atom().
stringer(V, _Nop) ->
    Buffer = io_lib:format("~p", [V]),
    lists:flatten(Buffer).


-spec sample() -> ignore.
sample() ->
    io:format("~p~n", [(stringer("Say Hello"))()]),
    io:format("~p~n", [(stringer('Say Hello'))()]),
    io:format("~p~n", [(stringer(123))()]).

%% --------------------------Another formate-------------------------%%

-define(STRINGER(V), {stringer, V}).

-spec stringer1(V) -> list() when
                               V :: atom()|list().
stringer1({stringer, V}) when is_list(V) ->
    stringer(V, nop);
stringer1({stringer, V}) when is_atom(V) ->
    stringer(V, nop);
stringer1(_Other) ->
    not_implemented.



-spec sample1() -> ignore.
sample1() ->
    io:format("~p~n", [?STRINGER("Say Hello")]),
    io:format("~p~n", [?STRINGER('Say Hello')]),
    io:format("~p~n", [?STRINGER(123)]).


%%
%% Local Functions
%%
