
-module(module_mysql_test1).

-compile(export_all).
-record(mysql_test1, {key, type, term, string, term2, term3}).

select(FiledList, Conditions) ->
    FormatCond = where_condition_fromat(Conditions),
    Columns = string:join([atom_to_list(Key)||Key<-FiledList], ","),
    SQL = "SELECT " ++ Columns ++ " FROM mysql_test1 " ++ mysql_helper:pack_where(FormatCond),
    Res = mysql_client:select(mysql_test1, SQL),
    unpack_fields(Res, FiledList).

read(#mysql_test1{key = KEY, type = TYPE}) ->
    SQL = "SELECT * FROM mysql_test1 "++" WHERE `key` = "++mysql_helper:pack_value_by_type({KEY,int})++" AND `type` = "++mysql_helper:pack_value_by_type({TYPE,int})++"",
    Res = mysql_client:read(mysql_test1, SQL),
    unpack_data(Res, []).

read(KEY, TYPE) ->
    SQL = "SELECT * FROM mysql_test1 "++" WHERE `key` = "++mysql_helper:pack_value_by_type({KEY,int})++" AND `type` = "++mysql_helper:pack_value_by_type({TYPE,int})++"",
    Res = mysql_client:read(mysql_test1, SQL),
    unpack_data(Res, []).

insert({mysql_test1, KEY, TYPE, TERM, STRING, TERM2, TERM3}) ->
    mysql_client:insert(mysql_test1, "INSERT INTO mysql_test1(`key`, `type`, `term`, `string`, `term2`, `term3`) VALUES ("++mysql_helper:pack_value_by_type({KEY,int})++", "++mysql_helper:pack_value_by_type({TYPE,int})++", "++mysql_helper:pack_value_by_type({TERM,blob})++", "++mysql_helper:pack_value_by_type({STRING,varchar})++", "++mysql_helper:pack_value_by_type({TERM2,term_varchar})++", "++mysql_helper:pack_value_by_type({TERM3,timestamp})++");");
insert([#mysql_test1{}|_]=INSERTS) ->
	SQL = pack_bash_insert(INSERTS),
	mysql_client:insert(mysql_test1, SQL);
insert([]) ->
	nothing.


update_fields_by_record({mysql_test1, KEY, TYPE, TERM, STRING, TERM2, TERM3}, Conditions) ->
    FormateCondition = where_condition_fromat(Conditions),
	SQL = "UPDATE mysql_test1 SET `term` = "++mysql_helper:pack_value_by_type({TERM,blob})++", `string` = "++mysql_helper:pack_value_by_type({STRING,varchar})++", `term2` = "++mysql_helper:pack_value_by_type({TERM2,term_varchar})++", `term3` = "++mysql_helper:pack_value_by_type({TERM3,timestamp})++" WHERE `key` = "++mysql_helper:pack_value_by_type({KEY,int})++" AND `type` = "++mysql_helper:pack_value_by_type({TYPE,int})++""++mysql_helper:pack_where(FormateCondition),
    mysql_client:update(mysql_test1, SQL).


update_fields(FieldValueList, Conditions) ->
    FormateCondition = where_condition_fromat(Conditions),
    FieldValueListTemp = where_condition_fromat(FieldValueList),
	SQL = "UPDATE mysql_test1 "++ mysql_helper:pack_update_columns(FieldValueListTemp)++mysql_helper:pack_where(FormateCondition),
    mysql_client:update(mysql_test1, SQL).

delete({mysql_test1, KEY, TYPE, TERM, STRING, TERM2, TERM3}) ->
	remove({mysql_test1, KEY, TYPE, TERM, STRING, TERM2, TERM3});
delete(Conditions) when is_list(Conditions) ->
    FormateCondition = where_condition_fromat(Conditions),
	SQL = "DELETE FROM mysql_test1 "++mysql_helper:pack_where(FormateCondition),
    mysql_client:remove(mysql_test1, SQL).

remove({mysql_test1, KEY, TYPE, TERM, STRING, TERM2, TERM3}) ->
    SQL = "DELETE FROM mysql_test1 WHERE `key` = "++mysql_helper:pack_value_by_type({KEY,int})++" AND `type` = "++mysql_helper:pack_value_by_type({TYPE,int})++"",
    mysql_client:remove(mysql_test1, SQL).

find(Conditions) ->
    find(Conditions, [], undefined).

%%
%% conditions: [{roleid, '=', 1}, {type, '!=', 3}]
find(Conditions, Limit, OrderBy) ->
	FormateCondition = where_condition_fromat(Conditions),
	SQL = "SELECT * FROM "++atom_to_list(mysql_test1)
							++mysql_helper:pack_where(FormateCondition)
							++mysql_helper:pack_orderby(OrderBy)
                            ++mysql_helper:pack_limit(Limit),
	Res = mysql_client:select(mysql_test1, SQL),
	unpack_data(Res, []).

all() ->
	SQL = "SELECT * FROM " ++ atom_to_list(mysql_test1),
	Res = mysql_client:select(mysql_test1, SQL),
	unpack_data(Res, []).

get_key(Record) ->	Record#mysql_test1.key.get_type(Record) ->	Record#mysql_test1.type.get_term(Record) ->	Record#mysql_test1.term.get_string(Record) ->	Record#mysql_test1.string.get_term2(Record) ->	Record#mysql_test1.term2.get_term3(Record) ->	Record#mysql_test1.term3.

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
	Record = mysql_helper:unpack_row(mysql_test1, RecordFor),
    unpack_data(Tail, [Record#mysql_test1{term2=mysql_helper:string_to_term(Record#mysql_test1.term2)}|AccInfo]);
unpack_data([], AccInfo) ->
    lists:reverse(AccInfo).


where_condition_fromat(Conditions) ->
    [{Column, Con, {Val, get_column_datatype(Column)}}||{Column, Con, Val} <- Conditions].


get_column_datatype(Column) ->
    proplists:get_value(Column, column_datatype()).

column_datatype() ->
    [{key,int},	 {type,int},	 {term,blob},	 {string,varchar},	 {term2,term_varchar},	 {term3,timestamp}].

get_bash_insert_value_list({mysql_test1, KEY, TYPE, TERM, STRING, TERM2, TERM3}) ->
	"("++mysql_helper:pack_value_by_type({KEY,int})++", "++mysql_helper:pack_value_by_type({TYPE,int})++", "++mysql_helper:pack_value_by_type({TERM,blob})++", "++mysql_helper:pack_value_by_type({STRING,varchar})++", "++mysql_helper:pack_value_by_type({TERM2,term_varchar})++", "++mysql_helper:pack_value_by_type({TERM3,timestamp})++")".

pack_bash_insert(Inserts) ->
	Values = string:join([get_bash_insert_value_list(Record)||Record<-Inserts], ", "),
	"INSERT INTO "++erlang:atom_to_list(mysql_test1)++"(`key`, `type`, `term`, `string`, `term2`, `term3`) VALUES"++Values++";".

