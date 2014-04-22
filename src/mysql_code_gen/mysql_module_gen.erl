%% Author: Administrator
%% Created: 2014-4-22
%% Description: TODO: Add description to mysql_module_gen
-module(mysql_module_gen).

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
'module_template'() ->
"
-module($MODULENAME).

-compile(export_all).

-record($MODULENAME, $RECORDS).

select(FiledList, Conditions) ->
    FormatCond = where_condition_fromat(Conditions),
    Columns = string:join([atom_to_list(Key)||Key<-FiledList], \",\"),
    SQL = \"SELECT \" ++ Columns ++ \" FROM $DBNAME \" ++ mysql_helper:pack_where(FormatCond),
    mysql_client:select($DBNAME, SQL).


".

file_test() ->
    File = file:open("../log/mysql_test.erl", []).



%%
%% Local Functions
%%

