%% Author: Eric.yutao
%% Created: 2013-12-8
%% Description: TODO: Add description to gen_db_module
-module(gen_db_module).

%%
%% Include files
%%
-include("module_define.hrl").
%%
%% Exported Functions
%%
-export([]).
-compile(export_all).

%% -define(OUPT_FILE, )

%%
%% Macros
%%

%% Common Define
-define(EQUAL_FORMAT, "~s = ~s").

%% Head Define
-define(MODULE_FORMAT, "-module(~s).~n").
-define(EXPORT_ALL, "-compile(export_all).~n").
-define(RECORD_FORMAT, "-record(~s, {~s}).~n").

%% Select SQL Define
-define(SELECT_FORMAT, "select(FiledList, Conditions) ->~n").
-define(SELECT_BODY, "\tFormatCond = where_condition_fromat(Conditions),~n\tColumns = string:join([atom_to_list(Key)||Key<-FiledList], \",\"),~n").
-define(SELECT_SQL, "\tSQL = \"SELECT \" ++ Columns ++ \" FROM ~s\" ++ mysql_helper:pack_where(FormatCond),~n").
-define(SELECT_END, "\tmysql_client:select(role_han_grave_db, SQL).~n").

%% Read SQL Define
-define(READ_HEAD, "read(#~s{~s}) ->~n").
-define(READ_SQL, "\tSQL = \"SELECT * FROM ~s WHERE \" ~s,~n").
-define(READ_OPT, "\tmysql_client:read(~s, SQL).~n").

%%
%% API Functions
%%
read_config(Filename) when is_list(Filename) ->
    case file:consult(Filename) of
        {ok, [#module_define{module_name= ModuleName,
                             columns    = Cols,
                             primary_key= PrimaryKey,
                             index      = Index,
                             engine     = Engine}=Comment]} ->
            {ok, FileHandle} = file:open(atom_to_list(ModuleName)++".erl", [write]),
%%             io:format("******************* Comment ~p~n", [{Comment}]),
%%             io:format("******************* Comment ~p~n~p~n", [ModuleName, Cols]),
            gen_db_module_code(FileHandle, ModuleName, Cols, PrimaryKey, Index, Engine);
        {error, Reason} ->
            io:format("****************** Gen code error ~p~n", [Reason])
    end.


%% read(#role_han_grave_db{roleid = ROLEID}) ->
%%     SQL = "SELECT * FROM role_han_grave_db WHERE roleid = "++ mysql_helper:pack_value_by_type({ROLEID, bigint}),
%%     mysql_client:read(role_han_grave_db, SQL).

%%io_lib:format(Format, Data)
-define(FORMAT_COL, "\"~s = \" ++mysql_helper:pack_value_by_type({~s, ~s})").


gen_db_module_code(FileHandle, ModuleName, Cols, PrimaryKey, Index, Engine) ->
    ColumnsTemp = [atom_to_list(ColName)||#columns_define{col_name = ColName}<-Cols],
%%     ColType = [{Col, Type}||#columns_define{col_name = Col, type = Type}<-Cols],
%%     PriTemp = [atom_to_list(Key)||Key<-PrimaryKey],
    ColEquStr = string:join([ColString++" = "++string:to_upper(ColString)||ColString<-ColumnsTemp], ", "),
    RecordForStr = [ColString++" = "++string:to_upper(ColString)||ColString<-ColumnsTemp],
%%     io:format("ColumnsEqualString ~p~n", [{Cols, PrimaryKey}]),
    HeadString = make_head_string(FileHandle, ModuleName, ColumnsTemp),
    SelectString = make_select_string(FileHandle, ModuleName),
    ReadString = make_read_string(FileHandle, ModuleName, Cols, PrimaryKey, ColEquStr),
    io:format(HeadString).

%%
%% Local Functions
%%
make_head_string(FileHandle, ModuleName, ColumnsTemp) when is_list(ColumnsTemp) ->
    Columns = string:join(ColumnsTemp, ", "),
    io:format(FileHandle, ?MODULE_FORMAT, [ModuleName]),
    io:format(FileHandle, ?EXPORT_ALL, []),
    io:format(FileHandle, ?RECORD_FORMAT, [ModuleName, Columns]),
    ok.


make_select_string(FileHandle, ModuleName) ->
    io:format(FileHandle, ?SELECT_FORMAT, []),
    io:format(FileHandle, ?SELECT_BODY, []),
    io:format(FileHandle, ?SELECT_SQL, [ModuleName]),
    io:format(FileHandle, ?SELECT_END, []).

make_read_string(FileHandle, ModuleName, Cols, PrimaryTemp, RecordForStr) ->
    PriColType = [begin
                      case lists:keyfind(Key, #columns_define.col_name, Cols) of
                          #columns_define{col_name = Name, type = Type} ->
                            {Name, Type}
                      end
                  end||Key<-PrimaryTemp],
    PriTypeString = make_col_type_string(PriColType),
    io:format("PriColType ~p~n", [PriColType]),
    io:format(FileHandle, ?READ_HEAD, [ModuleName, RecordForStr]),
    io:format(FileHandle, ?READ_SQL, [ModuleName, PriTypeString]),
    io:format(FileHandle, ?READ_OPT, [ModuleName]).







make_col_type_string(ColTypes) when is_list(ColTypes) ->
    Temp = [begin
         ColNameS = atom_to_list(ColName),
         TypeS = atom_to_list(Type),
         "++ \""++ColNameS++" = \" ++ mysql_helper:pack_value_by_type({"++string:to_upper(ColNameS)++", "++TypeS++"})"
     end||{ColName, Type}<-ColTypes],
    string:join(Temp, " ++ \",\"").








