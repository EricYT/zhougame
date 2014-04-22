
-module($MODULENAME).

-compile(export_all).

-record($MODULENAME, {$RECORDS}).

select(FiledList, Conditions) ->
    FormatCond = where_condition_fromat(Conditions),
    Columns = string:join([atom_to_list(Key)||Key<-FiledList], ","),
    SQL = "SELECT " ++ Columns ++ " FROM $MODULENAME " ++ mysql_helper:pack_where(FormatCond),
    mysql_client:select($MODULENAME, SQL).

read(#$MODULENAME{$KEYVALUES}) ->
    SQL = "SELECT * FROM $MODULENAME WHERE $PACKKEYS,
    mysql_client:read($MODULENAME, SQL);
read($KEYS) ->
    SQL = "SELECT * FROM $MODULENAME WHERE $PACKKEYS,
    Res = mysql_client:read($MODULENAME, SQL),
    unpack_data(Res, []).

write(#$MODULENAME{$RECORDVALUES}) ->
    case mysql_client:read($MODULENAME, "SELECT * FROM $MODULENAME WHERE $PACKKEYS) of
        [] ->
            SQL = "INSERT INTO $MODULENAME ($RECORDS) VALUES($PACKVALUES),
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
