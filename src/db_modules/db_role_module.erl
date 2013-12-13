%% Author: eric.yutao
%% Created: 2013-12-1
%% Description: TODO: Add description to db_role_module
-module(db_role_module).

%%
%% Include files
%%

-record(role, {roleid, rolename, login_time, gender}).

-define(TABLE, " role ").

%%
%% Exported Functions
%%
-export([]).
-compile(export_all).

%%
%% API Functions
%%
read(#role{roleid = RoleId, rolename = RoleName, login_time = Level, gender = Gender}) ->
    Res = mysql_op:read("SELECT * FROM" ++ ?TABLE ++ "WHERE roleid = " ++ erlang:integer_to_list(RoleId)),
    io:format("db_role_module ~p~n", [{"SELECT * FROM" ++ ?TABLE ++ "WHERE roleid = " ++ erlang:integer_to_list(RoleId), Res}]),
    pack(Res).

remove(#role{roleid = RoleId, rolename = RoleName, login_time = Level, gender = Gender}) ->
    io:format("db_role_module ~p~n", ["DELETE * FROM" ++ ?TABLE ++ "WHERE roleid = " ++ erlang:integer_to_list(RoleId)]),
    mysql_op:write("DELETE * FROM" ++ ?TABLE ++ "WHERE roleid = " ++ erlang:integer_to_list(RoleId)).


%%
%% Local Functions
%%
pack([RoleId, RoleName, Level, Gender]) ->
    #role{roleid = RoleId, rolename = RoleName, login_time = Level, gender = Gender}.
