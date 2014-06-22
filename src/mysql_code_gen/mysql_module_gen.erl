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
    {ModuleInfos, _ProtoInfos} = mysql_config:read_config(),
    Content = formate_values(ModuleInfos, []),
    SqlContent = formate_sql(ModuleInfos, []),
    io:format(">>>>>>>>> ~p~n", [{?MODULE, ?LINE, SqlContent}]),
    try
        {ok, File} = file:open("../log/module_mysql_test.erl", [write]),
        {ok, SQLFile} = file:open("../log/mysql_schemal.sql", [write]),
        file:write(File, Content),
        file:write(SQLFile, SqlContent),
        file:close(File),
        file:close(SQLFile)
    catch Error:Reason ->
            io:format(">>>>>>>>> ~p~n", [{ModuleInfos}])
    end.

formate_values([#module_define{module_name = ModuleName, columns = Cols, primary_key = PriKeys,
                              index = Indexs, engine = Eng}=_MoudleRecord|Tail], AccInfos) ->
    FileName = ?FilePre++erlang:atom_to_list(ModuleName),
    ModuleNameS = atom_to_list(ModuleName),
    ConvertFun =
        fun(#columns_define{col_name = Name, type = Type}, {AccTypeArgList, AccTypeArgS, AccArgList,
                                                            AccArgUps, AccArgs, AccArgsStr, AccArgsCol}) ->
                NameString = atom_to_list(Name),
                NameToUpper = string:to_upper(atom_to_list(Name)),
                {[{Name, Type}|AccTypeArgList],
                 [util:term_to_string({Name, Type})|AccTypeArgS],
                 [NameString++" = "++NameToUpper|AccArgList],
                 [NameToUpper|AccArgUps],
                 [NameString|AccArgs],
                 ["`"++NameString++"`"|AccArgsStr],
                 [Name|AccArgsCol]};
           (_, Values) ->
                Values
        end,
    {TypeArgList, TypeArgSTemp, ArgListTemp, ArgUpsTemp,
     ArgsTemp, ArgsStrTemp, ArgsCol} =
        lists:foldr(ConvertFun, {[], [], [], [], [], [], []}, Cols),
    TypeArgS= string:join(TypeArgSTemp, ",\r\t "),
    ArgList = string:join(ArgListTemp, ", "),
    ArgUps  = string:join(ArgUpsTemp, ", "),
    Args    = string:join(ArgsTemp, ", "),
	ArgsStr = string:join(ArgsStrTemp, ", "),
    {KeyValuesStrings, Keys} = formate_key_values(PriKeys, Cols),
	This = "{"++ModuleNameS++", "++ArgUps++"}",
	ValuesOfInsert = pack_values_of_insert0(TypeArgList),
    PrimaryKeys = pack_keys(PriKeys, TypeArgList),
    RecordGetCols = pack_get_record(ModuleName, TypeArgList),
	
	UnpackRecord = unpack_record(atom_to_list(ModuleName), TypeArgList),
    ValueTest = pack_insert(atom_to_list(ModuleName), ArgsStr, TypeArgList),
    UpdateData = pack_update0(atom_to_list(ModuleName), PriKeys, ArgsCol, TypeArgList),
    DeleteData = pack_delete0(atom_to_list(ModuleName), PriKeys, TypeArgList),
	
    TestReplace = mysql_op_gen:key_value_replace([{"$FILENAME", FileName},
                                                  {"$RECORDS", Args},
                                                  {"$MODULENAME", ModuleNameS},
                                                  {"$KEYVALUES", KeyValuesStrings},
                                                  {"$KEYS", Keys},
                                                  {"$THIS", This},
                                                  {"$RESTR", ArgsStr},
                                                  {"?ValuesOfInsertSqlString", ValuesOfInsert},
                                                  {"$RECORDVALUES", ArgList},
                                                  {"$UNPACKRECORD", UnpackRecord},
                                                  {"$RECORDGETCOLS", RecordGetCols},
                                                  {"$UPDATESQL", UpdateData},
                                                  {"$DELETESQL", DeleteData},
                                                  {"$SQL_INSERT0", ValueTest},
                                                  {"$PACKKEYS", PrimaryKeys},
                                                  {"$RECORDDEFINES", TypeArgS}
                                                 ], 'module_template'()),
    formate_values(Tail, [TestReplace|AccInfos]);
formate_values([], AccInfos) ->
    lists:reverse(AccInfos).


formate_sql([#module_define{module_name = ModuleName, columns = Cols,
                            primary_key = PriKeys, index = Indexs,
                            engine = Eng}=_MoudleRecord|Tail], AccInfos) ->
    TableName = erlang:atom_to_list(ModuleName),
    Sqls = formate_sql_by_colums(Cols, []),
    PriKeyString = formate_sql_primary_keys(PriKeys),
    IndexString = formate_sql_index(Indexs),
    if
        PriKeys =/= [] ->
            SqlPrimay = Sqls++",\n"++PriKeyString;
        true ->
            SqlPrimay = Sqls
    end,
    if
        Indexs =/= [] ->
            SqlIndex = SqlPrimay++",\n"++IndexString;
        true ->
            SqlIndex = SqlPrimay
    end,
    SqlContent =
        mysql_op_gen:key_value_replace([{"$TABLE_NAME", TableName},
                                        {"$TABLE_COMMENT", SqlIndex},
                                        {"$DB_ENGINE", atom_to_list(Eng)},
                                        {"$DB_CHARSET", "'utf8'"}],
                                        'mysql_schemal_template'()),
    formate_sql(Tail, [SqlContent|AccInfos]);
formate_sql([], AccInfos) ->
    string:join(lists:reverse(AccInfos), "\n\n\n").

%% `roleid` bigint(255) NOT NULL DEFAULT '0' COMMENT 'sdfsadfsdf'
%% `rolename` blob NOT NULL
%%  PRIMARY KEY (`aaa`,`2`)
%%  KEY `index` (`2`)
formate_sql_by_colums([#columns_define{type=term_varchar}=ColRecord|Tail], AccInfos) ->
    formate_sql_by_colums([ColRecord#columns_define{type=varchar}|Tail], AccInfos);
formate_sql_by_colums([#columns_define{type=blob}=ColRecord|Tail], AccInfos) ->
    #columns_define{col_name=ColName, type=Type, length=Len, is_null=IsNull,
                    default=Default, description=Des}=ColRecord,
    Col = "\`"++atom_to_list(ColName)++"\` blob ",
    if
        IsNull =:= 'notnull' ->
            Col1 = Col++"NOT NULL ";
        true ->
            Col1 = Col++"NULL "
    end,
    Col2 = Col1++"COMMENT '"++Des++"'",
    formate_sql_by_colums(Tail, [Col2|AccInfos]);
formate_sql_by_colums([#columns_define{}=ColRecord|Tail], AccInfos) ->
    #columns_define{col_name=ColName, type=Type, length=Len, is_null=IsNull,
                    default=Default, description=Des}=ColRecord,
    Col = "\`"++atom_to_list(ColName)++"` "++atom_to_list(Type)++"("++
              integer_to_list(Len)++") ",
    if
        IsNull =:= 'notnull' ->
            Col1 = Col++"NOT NULL ";
        true ->
            Col1 = Col++"NULL "
    end,
    if
        Default =:= []; Default =:= "" ->
            Col2 = Col1;
        true ->
            Col2 = Col1++"DEFAULT '"++util:term_to_string(Default)++"' "
    end,
    Col3 = Col2++"COMMENT '"++Des++"'",
    formate_sql_by_colums(Tail, [Col3|AccInfos]);
formate_sql_by_colums([], AccInfos) ->
    string:join(lists:reverse(""++AccInfos), ", \n\t").

formate_sql_primary_keys([]) ->
    "";
formate_sql_primary_keys(PrimaryKeys) ->
    KeysTems = ["`"++atom_to_list(Key)++"`"||Key<-PrimaryKeys],
    Keys = string:join(KeysTems, ","),
    "\tPRIMARY KEY ("++Keys++")".

formate_sql_index([]) ->
    "";
formate_sql_index(Indexs) ->
    IndexTemp = 
        lists:map(fun(Index) ->
                          "\tKEY `"++atom_to_list(Index)++"`"++
                          " (`"++atom_to_list(Index)++"`)"
                  end, Indexs),
    IndexString = string:join(IndexTemp, ","),
    IndexString.

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

pack_delete0(ModuleName, PriKeys, TypeArgList) ->
	Conditions = [{Key, '=', proplists:lookup(Key, TypeArgList)}||Key<-PriKeys],
	"DELETE FROM "++ModuleName++pack_where(Conditions).

pack_update0(ModuleName, PriKeys, ModuleAttrs, TypeArgList) ->
	WhereCon = [{Key, '=', proplists:lookup(Key, TypeArgList)}||Key<-PriKeys],
	UpdateCon = [{Name, '=', proplists:lookup(Name, TypeArgList)}||Name<-(ModuleAttrs--PriKeys)],
	"UPDATE "++ModuleName++pack_update_columns(UpdateCon)++pack_where(WhereCon).

pack_keys(PriKeys, TypeArgList) ->
    Keys = [{Name, '=', proplists:lookup(Name, TypeArgList)}||Name<-PriKeys],
	pack_where(Keys).

pack_get_record(ModuleName, TypeArgList) ->
    PackValues = ["get_"++atom_to_list(Name)++"(Record) ->\r\tRecord#"++atom_to_list(ModuleName)++"."++atom_to_list(Name)++"."
                  ||{Name, _Type}<-TypeArgList],
    string:join(PackValues, "\r\r").

unpack_record(ModuleName, TypeArgList) ->
	FilterFun = fun({Name, Type}, Acc) when Type =:= term_varchar; Type =:= term_char ->
					  TypeString = atom_to_list(Name),
					  [TypeString++"=mysql_helper:string_to_term(Record#"++ModuleName++"."++TypeString++")"
					  |Acc];
				   (_, Acc) ->
						Acc
				end,
	StringList = lists:foldr(FilterFun, [], TypeArgList),
	"Record#"++ModuleName++"{"++string:join(StringList, ",\r\t\t\t\t\t\t")++"}".

value_format({Name, Type}) ->
	"{"++string:to_upper(atom_to_list(Name))++","++atom_to_list(Type)++"}".

pack_where(Conditions) ->
    SQL = pack_kv(Conditions, []),
    case SQL of
        [] -> "";
        _ -> " WHERE " ++ string:join(SQL, " AND ")
    end.

pack_kv([], SQL) ->
    lists:reverse(SQL);
pack_kv([{ColumnName, '=', Val}|Tail], SQL) ->
    New = "`"++atom_to_list(ColumnName)++"`"++" = "++"\"++mysql_helper:pack_value_by_type("++value_format(Val)++")++\"",
    pack_kv(Tail, [New|SQL]);
pack_kv([{ColumnName, '!=', Val}|Tail], SQL) ->
    New = "`"++atom_to_list(ColumnName)++"`"++" != "++ "\"++mysql_helper:pack_value_by_type("++value_format(Val)++")++\"",
    pack_kv(Tail, [New|SQL]);
pack_kv([{ColumnName, '>', Val}|Tail], SQL) ->
    New = "`"++atom_to_list(ColumnName)++"`"++" > "++ "\"++mysql_helper:pack_value_by_type("++value_format(Val)++")++\"",
    pack_kv(Tail, [New|SQL]);
pack_kv([{ColumnName, '>=', Val}|Tail], SQL) ->
    New = "`"++atom_to_list(ColumnName)++"`"++" >= "++ "\"++mysql_helper:pack_value_by_type("++value_format(Val)++")++\"",
    pack_kv(Tail, [New|SQL]);
pack_kv([{ColumnName, '<', Val}|Tail], SQL) ->
    New = "`"++atom_to_list(ColumnName)++"`"++" < "++ "\"++mysql_helper:pack_value_by_type("++value_format(Val)++")++\"",
    pack_kv(Tail, [New|SQL]);
pack_kv([{ColumnName, '<=', Val}|Tail], SQL) ->
    New = "`"++atom_to_list(ColumnName)++"`"++" <= "++ "\"++mysql_helper:pack_value_by_type("++value_format(Val)++")++\"",
    pack_kv(Tail, [New|SQL]);
pack_kv([{ColumnName, 'in', Val}|Tail], SQL) ->
    New = "`"++atom_to_list(ColumnName)++"`"++" IN "++ "\"++mysql_helper:pack_value_by_type("++value_format(Val)++")++\"",
    pack_kv(Tail, [New|SQL]);
pack_kv([{ColumnName, 'not in', Val}|Tail], SQL) ->
    New = "`"++atom_to_list(ColumnName)++"`"++" NOT IN "++"\"++mysql_helper:pack_value_by_type("++value_format(Val)++")++\"",
    pack_kv(Tail, [New|SQL]).


pack_update_columns(Columns) ->
    KV = pack_kv(Columns, []),
    case length(KV) of
        0 -> "";
        _Any -> " SET " ++ string:join(KV, ", ")
    end.


'mysql_schemal_template'() ->
"
-- ----------------------------
-- Table structure for `$TABLE_NAME`
-- ----------------------------
DROP TABLE IF EXISTS `$TABLE_NAME`;
CREATE TABLE `$TABLE_NAME` (
    $TABLE_COMMENT
) ENGINE=$DB_ENGINE DEFAULT CHARSET=$DB_CHARSET;

".


'module_template'() ->
"
-module($FILENAME).

-compile(export_all).
-record($MODULENAME, {$RECORDS}).

select(FiledList, Conditions) ->
    FormatCond = where_condition_fromat(Conditions),
    Columns = string:join([atom_to_list(Key)||Key<-FiledList], \",\"),
    SQL = \"SELECT \" ++ Columns ++ \" FROM $MODULENAME \" ++ mysql_helper:pack_where(FormatCond),
    Res = mysql_client:select($MODULENAME, SQL),
    unpack_fields(Res, FiledList).

read(#$MODULENAME{$KEYVALUES}) ->
    SQL = \"SELECT * FROM $MODULENAME \"++\"$PACKKEYS\",
    Res = mysql_client:read($MODULENAME, SQL),
    unpack_data(Res, []).

read($KEYS) ->
    SQL = \"SELECT * FROM $MODULENAME \"++\"$PACKKEYS\",
    Res = mysql_client:read($MODULENAME, SQL),
    unpack_data(Res, []).

insert($THIS) ->
    mysql_client:insert($MODULENAME, \"$SQL_INSERT0\");
insert([#$MODULENAME{}|_]=INSERTS) ->
	SQL = pack_bash_insert(INSERTS),
	mysql_client:insert($MODULENAME, SQL);
insert([]) ->
	nothing.


update_fields_by_record($THIS, Conditions) ->
    FormateCondition = where_condition_fromat(Conditions),
	SQL = \"$UPDATESQL\"++mysql_helper:pack_where(FormateCondition),
    mysql_client:update($MODULENAME, SQL).


update_fields(FieldValueList, Conditions) ->
    FormateCondition = where_condition_fromat(Conditions),
    FieldValueListTemp = where_condition_fromat(FieldValueList),
	SQL = \"UPDATE $MODULENAME \"++ mysql_helper:pack_update_columns(FieldValueListTemp)++mysql_helper:pack_where(FormateCondition),
    mysql_client:update($MODULENAME, SQL).

delete($THIS) ->
	remove($THIS);
delete(Conditions) when is_list(Conditions) ->
    FormateCondition = where_condition_fromat(Conditions),
	SQL = \"DELETE FROM $MODULENAME \"++mysql_helper:pack_where(FormateCondition),
    mysql_client:remove($MODULENAME, SQL).

remove($THIS) ->
    SQL = \"$DELETESQL\",
    mysql_client:remove($MODULENAME, SQL).

find(Conditions) ->
    find(Conditions, [], undefined).

%%
%% conditions: [{roleid, '=', 1}, {type, '!=', 3}]
find(Conditions, Limit, OrderBy) ->
	FormateCondition = where_condition_fromat(Conditions),
	SQL = \"SELECT * FROM \"++atom_to_list($MODULENAME)
							++mysql_helper:pack_where(FormateCondition)
							++mysql_helper:pack_orderby(OrderBy)
                            ++mysql_helper:pack_limit(Limit),
	Res = mysql_client:select($MODULENAME, SQL),
	unpack_data(Res, []).

all() ->
	SQL = \"SELECT * FROM \" ++ atom_to_list($MODULENAME),
	Res = mysql_client:select($MODULENAME, SQL),
	unpack_data(Res, []).

$RECORDGETCOLS

unpack_fields(Fields, FieldNames) ->
    FieldNamesTemp = [get_column_datatype(Column)||Column<-FieldNames],
    ConvertFun = fun(varchar, {AccIndex, Acc}) ->
                        {AccIndex+1, [AccIndex|Acc]};
                    (term_varchar, {AccIndex, Acc}) ->
                        {AccIndex+1, [AccIndex|Acc]};
                    (_Other, {AccIndex, Acc}) ->
                        {AccIndex+1, Acc}
                 end,
    {_, FieldList} = lists:foldl(ConvertFun, {1, []}, FieldNamesTemp),
    unpack_fields(Fields, FieldList, []).

unpack_fields(Res, [], _) ->
    Res;
unpack_fields([], _, Acc) ->
    lists:reverse(Acc);
unpack_fields([Fields|Tail], FieldList, Acc) ->
    unpack_fields(Tail, FieldList, [unpack_field1(Fields, [], 1, FieldList)|Acc]).

unpack_field1([Field|Tail], Acc, Index, FieldList) ->
    case lists:member(Index, FieldList) of
        true ->
            unpack_field1(Tail, [mysql_helper:string_to_term(Field)|Acc], Index+1, FieldList);
        false ->
            unpack_field1(Tail, [Field|Acc], Index+1, FieldList)
    end;
unpack_field1([], Acc, _, _) ->
    lists:reverse(Acc).

unpack_data([RecordFor|Tail], AccInfo) ->
	Record = mysql_helper:unpack_row($MODULENAME, RecordFor),
    unpack_data(Tail, [$UNPACKRECORD|AccInfo]);
unpack_data([], AccInfo) ->
    lists:reverse(AccInfo).


where_condition_fromat(Conditions) ->
    [{Column, Con, {Val, get_column_datatype(Column)}}||{Column, Con, Val} <- Conditions].


get_column_datatype(Column) ->
    proplists:get_value(Column, column_datatype()).

column_datatype() ->
    [$RECORDDEFINES].

get_bash_insert_value_list($THIS) ->
	\"?ValuesOfInsertSqlString\".

pack_bash_insert(Inserts) ->
	Values = string:join([get_bash_insert_value_list(Record)||Record<-Inserts], \", \"),
	\"INSERT INTO \"++erlang:atom_to_list($MODULENAME)++\"($RESTR) VALUES\"++Values++\";\".

".

