%% Author: Administrator
%% Created: 2013-12-1
%% Description: TODO: Add description to mysql_helper
-module(mysql_helper).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([]).
-compile(export_all).

%%
%% API Functions
%%

%% Old unpack function,zip the type and value
unpacks(Values) when erlang:is_list(Values) ->
    unpacks(Values, []).

unpacks([], Res) ->
    lists:reverse(Res);
unpacks([Term|Tail], Res) when erlang:is_binary(Term) ->
    unpacks(Tail, [erlang:binary_to_list(Term)|Res]);
unpacks([Term|Tail], Res) ->
    unpacks(Tail, [Term|Res]).

unpack(FiledInfos, Rows) when erlang:is_list(FiledInfos), erlang:is_list(Rows) ->
%%     ZipRes = lists:zip(FiledInfos, Rows),
    Res = unpacks(Rows),
    io:format("mysql helper ~p~n", [{Res}]).
%%     unpack1(ZipRes, []).

unpack1([], Res) ->
    lists:reverse(Res);
unpack1([{{_, _, _, Type}, Value}|Tail], Res) when Type =:= 'LONGLONG';
                                                   Type =:= 'SHORT',
                                                   is_integer(Value) ->
    unpack1(Tail, [Value|Res]);
unpack1([{{_, _, _, Type}, Value}|Tail], Res) when Type =:= 'VAR_STRING';
                                                   Type =:= 'STRING';
                                                   Type =:= 'BLOB',
                                                   is_binary(Value) ->
    unpack1(Tail, [erlang:binary_to_list(Value)|Res]);
unpack1([{{_, _, _, Type}, Value}|Tail], Res) when Type =:= 'TIMESTAMP',
                                                   is_tuple(Value) ->
    {datetime, Time} = Value,
    TimeStamp = time_util:date_time_to_now(Time),
    unpack1(Tail, [TimeStamp|Res]);
unpack1([_], Res) ->
    error.


%%
%% Local Functions
%%

string_to_term("") ->
    [];
string_to_term(Val) ->
    {ok, Term} = util:string_to_term(Val),
    Term.

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
pack_kv([{ColumnName, '<=', Val}|Tail], SQL) ->
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

pack_orderby(undefined) ->
    "";
pack_orderby({Column, asc}) ->
    " ORDER BY "++atom_to_list(Column)++" ASC";
pack_orderby({Column, desc}) ->
    " ORDER BY "++atom_to_list(Column)++" DESC".

pack_limit([]) ->
    "";
pack_limit({Num1, Num2}) ->
    " LIMIT "++integer_to_list(Num1)++","++integer_to_list(Num2);
pack_limit(Num) ->
    " LIMIT "++integer_to_list(Num).

unpack_row(Module, RowColumnDataList) ->
	list_to_tuple([Module|RowColumnDataList]).

