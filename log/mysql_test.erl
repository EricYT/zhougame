
-module(module_mysql_test).

-compile(export_all).

-record(mysql_test, {key, type, term, string, term2}).

select(FiledList, Conditions) ->
    FormatCond = where_condition_fromat(Conditions),
    Columns = string:join([atom_to_list(Key)||Key<-FiledList], ","),
    SQL = "SELECT " ++ Columns ++ " FROM mysql_test " ++ mysql_helper:pack_where(FormatCond),
    mysql_client:select(mysql_test, SQL).

read(#mysql_test{key = KEY, type = TYPE}) ->
    SQL = "SELECT * FROM mysql_test WHERE "++$PACKKEYS,
    mysql_client:read(mysql_test, SQL);
read(KEY, TYPE) ->
    SQL = "SELECT * FROM mysql_test WHERE "++$PACKKEYS,
    Res = mysql_client:read(mysql_test, SQL),
    unpack_data(Res, []).

insert({mysql_test, key = KEY, type = TYPE, term = TERM, string = STRING, term2 = TERM2}) ->
    mysql_client:insert(mysql_test, "INSERT INTO mysql_test(`key`, `type`, `term`, `string`, `term2`) VALUES ("++mysql_helper:pack_value_by_type({KEY,int})++", "++mysql_helper:pack_value_by_type({TYPE,int})++", "++mysql_helper:pack_value_by_type({TERM,term_varchar})++", "++mysql_helper:pack_value_by_type({STRING,varchar})++", "++mysql_helper:pack_value_by_type({TERM2,term_varchar})++");");
insert([#mysql_test{}|_]=INSERTS) ->
	SQL = pack_bash_insert(INSERTS),
	mysql_client:insert(mysql_test, SQL);
insert([]) ->
	nothing.


unpack_data([[#VALUESRECORD]|Tail], AccInfo) ->
    unpack_data(Tail, [mysql_test{$VALUESPACKS}|AccInfo]);
unpack_data([], AccInfo) ->
    lists:reverse(AccInfo).


where_condition_fromat(Conditions) ->
    [{Column, Con, {Val, get_column_datatype(Column)}}||{Column, Con, Val} <- Conditions].


get_column_datatype(Column) ->
    proplists:get_value(Column, column_datatype()).

column_datatype() ->
    [{{key,int},

get_bash_insert_value_list({mysql_test, key = KEY, type = TYPE, term = TERM, string = STRING, term2 = TERM2}) ->
	"("++mysql_helper:pack_value_by_type({KEY,int})++", "++mysql_helper:pack_value_by_type({TYPE,int})++", "++mysql_helper:pack_value_by_type({TERM,term_varchar})++", "++mysql_helper:pack_value_by_type({STRING,varchar})++", "++mysql_helper:pack_value_by_type({TERM2,term_varchar})++")".

pack_bash_insert(Inserts) ->
	Values = string:join([get_bash_insert_value_list(Record)||Record<-Inserts], ", "),
	"INSERT INTO "++erlang:atom_to_list(mysql_test)++"(`key`, `type`, `term`, `string`, `term2`) VALUES"++Values++";".
