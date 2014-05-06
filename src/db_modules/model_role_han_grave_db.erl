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
    Res = mysql_client:read(role_han_grave_db, SQL),
    unpack_data(Res, []).

insert(#role_han_grave_db{roleid = ROLEID, monster_info = MONSTER_INFO, count = COUNT, update_time = UPDATE_TIME, last_call_quality = LAST_CALL_QUALITY}) ->
    SQL = "INSERT INTO role_han_grave_db (`roleid`, `monster_info`, `count`, `update_time`, `last_call_quality`) VALUES(" ++ mysql_helper:pack_value_by_type({ROLEID, bigint}) ++ " ," ++ mysql_helper:pack_value_by_type({MONSTER_INFO, term_varchar}) ++ " ," ++ mysql_helper:pack_value_by_type({COUNT, int}) ++ " ," ++ mysql_helper:pack_value_by_type({UPDATE_TIME, timestamp}) ++ " ," ++ mysql_helper:pack_value_by_type({LAST_CALL_QUALITY, int}) ++");",
    io:format("****************~n"),
    mysql_client:insert(role_han_grave_db, SQL).


unpack_data([[RoleId, MonsterInfo, Count, UpdateTime, LastCallQuality]|Tail], AccInfo) ->
    unpack_data(Tail, [#role_han_grave_db{roleid           = RoleId,
                                          monster_info     = mysql_helper:string_to_term(MonsterInfo),
                                          count            = Count,
                                          update_time      = UpdateTime,
                                          last_call_quality= LastCallQuality}|AccInfo]);
unpack_data([], AccInfo) ->
    lists:reverse(AccInfo).


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