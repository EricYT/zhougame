
write(#$MODULENAME{$RECORDVALUES}) ->
    case mysql_client:read($MODULENAME, $PACKKEYS) of
        [] ->
            mysql_client:write(role_han_grave_db, "INSERT INTO mysql_test(1, 2, 3) VALUES ("++mysql_helper:pack_value_by_type({KEY,int})++", "++mysql_helper:pack_value_by_type({TYPE,int})++", "++mysql_helper:pack_value_by_type({TERM,term_varchar})++", "++mysql_helper:pack_value_by_type({STRING,varchar})++", "++mysql_helper:pack_value_by_type({TERM2,term_varchar})++");");
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
