-module(model_role_han_grave_db).

-compile(export_all).

-record(role_han_grave_db, {roleid, monster_info, count, update_time, last_call_quality}).

select(FiledList, Conditions) ->
	FormatCond = where_condition_fromat(Conditions),
	Columns = string:join([atom_to_list(Key)||Key<-FiledList], ","),
	SQL = "SELECT " ++ Columns ++ " FROM role_han_grave_db" ++ mysql_helper:pack_where(FormatCond),
	mysql_client:select(role_han_grave_db, SQL).

read(#role_han_grave_db{roleid = ROLEID}) ->
    SQL = "SELECT * FROM role_han_grave_db WHERE roleid = "++ mysql_helper:pack_value_by_type({ROLEID, bigint}),
    mysql_client:read(role_han_grave_db, SQL);
read(ROLEID) ->
    SQL = "SELECT * FROM role_han_grave_db WHERE roleid = "++ mysql_helper:pack_value_by_type({ROLEID, bigint}),
    mysql_client:read(role_han_grave_db, SQL).

write(#role_han_grave_db{roleid = ROLEID, monster_info = MONSTER_INFO, count = COUNT, update_time = UPDATE_TIME, last_call_quality = LAST_CALL_QUALITY}) ->
    case mysql_client:read(role_han_grave_db, "SELECT * FROM role_han_grave_db WHERE roleid = "++integer_to_list(ROLEID)) of
        [] ->
%%             SQL = "INSERT INTO role_han_grave_db VALUES(" ++ mysql_helper:pack_value_by_type({ROLEID, bigint}) ++ mysql_helper:pack_value_by_type({MONSTER_INFO, term_varchar}) ++ mysql_helper:pack_value_by_type({COUNT, int}) ++ mysql_helper:pack_value_by_type({UPDATE_TIME, timestamp}) ++ mysql_helper:pack_value_by_type({LAST_CALL_QUALITY, int}) ++");",
			SQL = "INSERT INTO role_han_grave_db (`roleid`, `monster_info`, `count`, `update_time`, `last_call_quality`) VALUES(" ++ mysql_helper:pack_value_by_type({ROLEID, bigint}) ++ " ," ++ mysql_helper:pack_value_by_type({MONSTER_INFO, term_varchar}) ++ " ," ++ mysql_helper:pack_value_by_type({COUNT, int}) ++ " ," ++ mysql_helper:pack_value_by_type({UPDATE_TIME, timestamp}) ++ " ," ++ mysql_helper:pack_value_by_type({LAST_CALL_QUALITY, int}) ++");",
			io:format("****************~n"),
            mysql_client:write(role_han_grave_db, SQL);
        _ ->
%%             SQL = "UPDATE role_han_grave_db SET " ++ "roleid = " ++ mysql_helper:pack_value_by_type({ROLEID, bigint}) ++ " monster_info = " ++ mysql_helper:pack_value_by_type({MONSTER_INFO, term_varchar}) ++ mysql_helper:pack_value_by_type({COUNT, int}) ++ mysql_helper:pack_value_by_type({UPDATE_TIME, timestamp}) ++ mysql_helper:pack_value_by_type({LAST_CALL_QUALITY, int})
            SQL = "",
            mysql_client:write(role_han_grave_db, SQL)
    end.


where_condition_fromat(Conditions) ->
	[{Column, Con, {Val, get_column_datatype(Column)}}||{Column, Con, Val} <- Conditions].


get_column_datatype(Column) ->
	proplists:get_value(Column, column_datatype()).

column_datatype() ->
	[{roleid, bigint},
	 {monster_info, term_varchar},
	 {count, int},
	 {update_time, timestamp},
	 {last_call_quality, int}].