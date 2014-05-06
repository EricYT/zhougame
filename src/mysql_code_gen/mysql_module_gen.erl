%% Author: Administrator
%% Created: 2014-4-22
%% Description: TODO: Add description to mysql_module_gen
-module(mysql_module_gen).

%%
%% Include files
%%
-include("module_define.hrl").

-define(FilePre, "module_").

-define(IF(C, A, B), case C of true -> A; _Other -> B end).

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
    Content = formate_values(ModuleInfos),
    {ok, File} = file:open("../log/mysql_test.erl", [write]),
%%     io:format(">>>>>>>>> ~p~n", [{Content}]),
    file:write(File, Content).

formate_values(#module_define{module_name = ModuleName, columns = Cols, primary_key = PriKeys,
                              index = Indexs, engine = Eng}=_MoudleRecord) ->
    FileName = ?FilePre++erlang:atom_to_list(ModuleName),
    ModuleNameS = atom_to_list(ModuleName),
    ConvertFun =
        fun(#columns_define{col_name = Name, type = Type}, {AccTypeArgList, AccTypeArgS, AccArgList,
                                                            AccArgUps, AccArgs, AccArgsStr}) ->
                NameString = atom_to_list(Name),
                NameToUpper = string:to_upper(atom_to_list(Name)),
                {[{Name, Type}|AccTypeArgList],
                 [util:term_to_string({Name, Type})|AccTypeArgS],
                 [NameString++" = "++NameToUpper|AccArgList],
                 [NameToUpper|AccArgUps],
                 [NameString|AccArgs],
				 ["`"++NameString++"`"|AccArgsStr]};
           (_, Values) ->
                Values
        end,
    {TypeArgList, TypeArgSTemp, ArgListTemp, ArgUpsTemp, ArgsTemp, ArgsStrTemp} =
        lists:foldr(ConvertFun, {[], [], [], [], [], []}, Cols),
    TypeArgS= string:join(TypeArgSTemp, ",\r\t "),
    ArgList = string:join(ArgListTemp, ", "),
    ArgUps  = string:join(ArgUpsTemp, ", "),
    Args    = string:join(ArgsTemp, ", "),
	ArgsStr = string:join(ArgsStrTemp, ", "),
    {KeyValuesStrings, Keys} = formate_key_values(PriKeys, Cols),
	This = "{"++ModuleNameS++", "++ArgList++"}",
	ValuesOfInsert = pack_values_of_insert0(TypeArgList),
    io:format(">>>>>>>>>>>>> ~p~n", [{TypeArgList, ArgList, ArgUps, Args, Keys, This, ArgsStr}]),
    ValueTest = pack_insert(atom_to_list(ModuleName), ArgsStr, TypeArgList),
    TestReplace = mysql_op_gen:key_value_replace([{"$FILENAME", FileName},
                                                  {"$RECORDS", Args},
                                                  {"$MODULENAME", ModuleNameS},
                                                  {"$KEYVALUES", KeyValuesStrings},
                                                  {"$KEYS", Keys},
												  {"$THIS", This},
												  {"$RESTR", ArgsStr},
												  {"?ValuesOfInsertSqlString", ValuesOfInsert},
                                                  {"$RECORDVALUES", ArgList},
                                                  {"$SQL_INSERT0", ValueTest},
                                                  {"$RECORDDEFINES", TypeArgS}
                                                 ], 'module_template'()),
    TestReplace.

formate_key_values(Keys, Records) ->
    ConvertFun =
        fun(Key, {AccKeys, AccKeyString}) ->
                case lists:keymember(Key, #columns_define.col_name, Records) of
                    true ->
                        KeyString = atom_to_list(Key),
                        KeyUpper = string:to_upper(atom_to_list(Key)),
                        {[KeyString++" = "++KeyUpper|AccKeys], [KeyUpper|AccKeyString]};
                    false ->
                        exit("Not Found Key")
                end;
           (_, Values) ->
                Values
        end,
    {KeyValues, KeyStringTemp} =
        lists:foldr(ConvertFun, {[], []}, Keys),
    {string:join(KeyValues, ", "), string:join(KeyStringTemp, ", ")}.


pack_insert(ModuleName, TableArgsString, TypeArgList) ->
	"INSERT INTO "++ModuleName++"("++TableArgsString++") VALUES "++pack_values_of_insert0(TypeArgList)++";".

pack_values_of_insert0(TypeArgList) ->
	Values = string:join(["\"++mysql_helper:pack_value_by_type("++value_format(Value)++")++\"" ||Value<-TypeArgList], ", "),
	"("++Values++")".

value_format({Name, Type}) ->
	"{"++string:to_upper(atom_to_list(Name))++","++atom_to_list(Type)++"}".


pack_value_by_type({Val, blob}) ->
    pack_value(term_to_binary(Val));
pack_value_by_type({Val, Type}) when Type =:= term_varchar;
                                     Type =:= term_char ->
    pack_value(util:term_to_string(Val));
pack_value_by_type({Val, _Type}) ->
    pack_value(Val).


pack_value(undefined) ->
    "null";
pack_value(true) ->
    "TRUE";
pack_value(false) ->
    "FALSE";
pack_value(Val) when is_atom(Val) ->
    pack_value(atom_to_list(Val));
pack_value(Val) when is_integer(Val) ->
    integer_to_list(Val);
pack_value(Val) when is_float(Val) ->
    float_to_list(Val);
pack_value({MegaSec, Sec, MicroSec}=Now) when is_integer(MegaSec),
                                          is_integer(Sec),
                                          is_integer(MicroSec) ->
    pack_datetime(Now);
pack_value({{_, _, _}, {_, _, _}}=Time) ->
    pack_datetime(Time);
pack_value(Val) when is_binary(Val) ->
    mysql:quote(binary_to_list(Val));
pack_value(Val) when is_list(Val) ->
    mysql:quote(Val).


pack_datetime(undefined) ->
    "null";
pack_datetime(0) ->
    "null";
pack_datetime({0, 0, 0}) ->
    "null";
pack_datetime({{Y, M, D}, {H, I, S}}) ->
    [format_time(X)||X<-[Y, M, D, H, I, S]];
pack_datetime({_, _, _}=Now) ->
    {{Y, M, D}, {H, I, S}} = calendar:now_to_local_time(Now), %% local time    datetime
    "'" ++ string:join([format_time(X)||X<-[Y, M, D]], "-") ++ " " ++ string:join([format_time(X)||X<-[H, I, S]], ":") ++ "'".

format_time(Val) when Val < 10 ->
    "0" ++ integer_to_list(Val);
format_time(Val) ->
    integer_to_list(Val).


pack_where(Conditions) ->
    SQL = pack_kv(Conditions, []),
    case SQL of
        [] -> "";
        _ -> " WHERE " ++ string:join(SQL, " AND ")
    end.

pack_kv([], SQL) ->
    lists:reverse(SQL);
pack_kv([{ColumnName, '=', Val}|Tail], SQL) ->
    New = atom_to_list(ColumnName) ++ " = " ++ pack_value_by_type(Val),
    pack_kv(Tail, [New|SQL]);
pack_kv([{ColumnName, '!=', Val}|Tail], SQL) ->
    New = atom_to_list(ColumnName)++" != "++pack_value_by_type(Val),
    pack_kv(Tail, [New|SQL]);
pack_kv([{ColumnName, '>', Val}|Tail], SQL) ->
    New = atom_to_list(ColumnName)++" > "++pack_value_by_type(Val),
    pack_kv(Tail, [New|SQL]);
pack_kv([{ColumnName, '>=', Val}|Tail], SQL) ->
    New = atom_to_list(ColumnName)++" >= "++pack_value_by_type(Val),
    pack_kv(Tail, [New|SQL]);
pack_kv([{ColumnName, '<', Val}|Tail], SQL) ->
    New = atom_to_list(ColumnName)++" < "++pack_value_by_type(Val),
    pack_kv(Tail, [New|SQL]);
pack_kv([{ColumnName, '!=', Val}|Tail], SQL) ->
    New = atom_to_list(ColumnName)++" <= "++pack_value_by_type(Val),
    pack_kv(Tail, [New|SQL]);
pack_kv([{ColumnName, 'in', Val}|Tail], SQL) ->
    New = atom_to_list(ColumnName)++" IN "++pack_value_by_type(Val),
    pack_kv(Tail, [New|SQL]);
pack_kv([{ColumnName, 'not in', Val}|Tail], SQL) ->
    New = atom_to_list(ColumnName)++" NOT IN "++pack_value_by_type(Val),
    pack_kv(Tail, [New|SQL]).


pack_update_columns(Columns) ->
    KV = pack_kv(Columns, []),
    case length(KV) of
        0 -> "";
        _Any -> " SET " ++ string:join(KV, ", ")
    end.


'module_template'() ->
"
-module($FILENAME).

-compile(export_all).

-record($MODULENAME, {$RECORDS}).

select(FiledList, Conditions) ->
    FormatCond = where_condition_fromat(Conditions),
    Columns = string:join([atom_to_list(Key)||Key<-FiledList], \",\"),
    SQL = \"SELECT \" ++ Columns ++ \" FROM $MODULENAME \" ++ mysql_helper:pack_where(FormatCond),
    mysql_client:select($MODULENAME, SQL).

read(#$MODULENAME{$KEYVALUES}) ->
    SQL = \"SELECT * FROM $MODULENAME WHERE \"++$PACKKEYS,
    mysql_client:read($MODULENAME, SQL);
read($KEYS) ->
    SQL = \"SELECT * FROM $MODULENAME WHERE \"++$PACKKEYS,
    Res = mysql_client:read($MODULENAME, SQL),
    unpack_data(Res, []).

insert($THIS) ->
    mysql_client:insert($MODULENAME, \"$SQL_INSERT0\");
insert([#$MODULENAME{}|_]=INSERTS) ->
	SQL = pack_bash_insert(INSERTS),
	mysql_client:insert($MODULENAME, SQL);
insert([]) ->
	nothing.


unpack_data([[#VALUESRECORD]|Tail], AccInfo) ->
    unpack_data(Tail, [$MODULENAME{$VALUESPACKS}|AccInfo]);
unpack_data([], AccInfo) ->
    lists:reverse(AccInfo).


where_condition_fromat(Conditions) ->
    [{Column, Con, {Val, get_column_datatype(Column)}}||{Column, Con, Val} <- Conditions].


get_column_datatype(Column) ->
    proplists:get_value(Column, column_datatype()).

column_datatype() ->
    [{$RECORDDEFINES}].

get_bash_insert_value_list($THIS) ->
	\"?ValuesOfInsertSqlString\".

pack_bash_insert(Inserts) ->
	Values = string:join([get_bash_insert_value_list(Record)||Record<-Inserts], \", \"),
	\"INSERT INTO \"++erlang:atom_to_list($MODULENAME)++\"($RESTR) VALUES\"++Values++\";\".

".



%%
%% Local Functions
%%



