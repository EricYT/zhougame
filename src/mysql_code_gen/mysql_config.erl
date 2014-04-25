%% Author: Administrator
%% Created: 2014-4-25
%% Description: TODO: read mysql config
-module(mysql_config).

%%
%% Include files
%%
-include("module_define.hrl").
-define(EXT, "../src/mysql_code_gen/*.config").

%%
%% Exported Functions
%%
-export([read_config/0]).

%%
%% API Functions
%%
-spec read_config() -> ignore.
read_config() ->
    Files = filelib:wildcard(?EXT),
    file_read_format(Files, [], []).

file_read_format([Filename|Tail], AccModules, AccProtos) ->
    case file:consult(Filename) of
        {ok, Terms} ->
            {Modules, Protos} = format_module_or_proto(Terms, [], []),
            file_read_format(Tail, Modules++AccModules, Protos++AccProtos);
        {error, Error} ->
            io:format(">>>>>>>>>>>>> ~p~n", [{?MODULE, ?LINE, Error, Filename}]),
            file_read_format(Tail, AccModules, AccProtos)
    end;
file_read_format([], AccModules, AccProtos) ->
    {AccModules, AccProtos}.

format_module_or_proto([#module_define{}=Record|Tail], ModuleInfos, ProtoInfos) ->
    format_module_or_proto(Tail, [Record|ModuleInfos], ProtoInfos);
format_module_or_proto([#proto_define{}=Record|Tail], ModuleInfos, ProtoInfos) ->
    format_module_or_proto(Tail, ModuleInfos, [Record|ProtoInfos]);
format_module_or_proto([], ModuleInfos, ProtoInfos) ->
    {ModuleInfos, ProtoInfos}.


%%
%% Local Functions
%%

