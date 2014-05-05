%% Author: Administrator
%% Created: 2014-4-22
%% Description: TODO: Add description to mysql_module_gen
-module(mysql_module_gen).

%%
%% Include files
%%
-include("module_define.hrl").

-define(FilePre, "module_").


%%
%% Exported Functions
%%
-export([]).
-compile(export_all).

%%
%% API Functions
%%
file_test() ->
    {[ModuleInfos], _ProtoInfos} = mysql_config:read_config(),
    Values = formate_values(ModuleInfos),
    {ok, File} = file:open("../log/mysql_test.erl", [write]),
    io:format(">>>>>>>>> ~p~n", [Values]),
    file:write(File, Values).
%%     file:write(File, 'module_template'()).

formate_values(#module_define{module_name = ModuleName, columns = Cols, primary_key = PriKeys,
                              index = Indexs, engine = Eng}=MoudleRecord) ->
    FileName = ?FilePre++erlang:atom_to_list(ModuleName),
    {Records, RecordValues, RecordTypes} = formate_records(Cols, [], [], []),
    KeyValues = formate_key_values(PriKeys, Records),
    NameTypes = formate_packs(RecordTypes, []),
    {Records, RecordValues, RecordTypes, NameTypes, KeyValues}.

formate_records([#columns_define{col_name = Name, type = Type, length = Len, is_null = IsNull,
                                 default = Def, description = Des}|Tail], AccCols, AccColsValues, AccColsTypes) ->
    RecordList = lists:concat([Name, " = ", string:to_upper(atom_to_list(Name))]),
    formate_records(Tail, [Name|AccCols], [RecordList|AccColsValues], [{Name, Type}|AccColsTypes]);
formate_records([], AccCols, AccColsValues, AccColsTypes) ->
    {lists:reverse(AccCols), string:join(lists:reverse(AccColsValues), ", "), lists:reverse(AccColsTypes)}.

formate_key_values(Keys, Records) ->
    string:join([begin
                     case lists:member(Key, Records) of
                         true ->
                             lists:concat([Key, " = ", string:to_upper(atom_to_list(Key))]);
                         false ->
                             throw("Not Found Key")
                     end
                 end||Key<-Keys], ", ").

formate_packs([{Name, Type}|Tail], AccNames) ->
    NameType = {string:to_upper(atom_to_list(Name)), Type},
    formate_packs(Tail, [NameType|AccNames]);
formate_packs([], AccNames) ->
    {lists:reverse(AccNames)}.




'module_template'() ->
"
-module($FILENAME).

-compile(export_all).

-record($MODULENAME, $RECORDS).

select(FiledList, Conditions) ->
    FormatCond = where_condition_fromat(Conditions),
    Columns = string:join([atom_to_list(Key)||Key<-FiledList], \",\"),
    SQL = \"SELECT \" ++ Columns ++ \" FROM $MODULENAME \" ++ mysql_helper:pack_where(FormatCond),
    mysql_client:select($MODULENAME, SQL).

read(#$MODULENAME{$KEYVALUES}) ->
    SQL = \"SELECT * FROM $MODULENAME WHERE $PACKKEYS,
    mysql_client:read($MODULENAME, SQL);
read($KEYS) ->
    SQL = \"SELECT * FROM $MODULENAME WHERE $PACKKEYS,
    Res = mysql_client:read($MODULENAME, SQL),
    unpack_data(Res, []).

write(#$MODULENAME{$RECORDVALUES}) ->
    case mysql_client:read($MODULENAME, \"SELECT * FROM $MODULENAME WHERE $PACKKEYS) of
        [] ->
            SQL = \"INSERT INTO $MODULENAME ($RECORDS) VALUES($PACKVALUES),
            mysql_client:write(role_han_grave_db, SQL);
        _ ->
            todo
    end.


unpack_data([[#VALUESRECORD]|Tail], AccInfo) ->
    unpack_data(Tail, [$MODULENAME{$VALUESPACKS}|AccInfo]);
unpack_data([], AccInfo) ->
    lists:reverse(AccInfo).


where_condition_fromat(Conditions) ->
    [{Column, Con, {Val, get_column_datatype(Column)}}||{Column, Con, Val} <- Conditions].


get_column_datatype(Column) ->
    proplists:get_value(Column, column_datatype()).

column_datatype() ->
    [$RECORDDEFINES].
".



%%
%% Local Functions
%%



