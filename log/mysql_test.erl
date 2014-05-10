%% Author: Administrator
%% Created: 2014-5-10
%% Description: TODO: Add description to mysql_test
-module(mysql_test).

%%
%% Include files
%%
-record(mysql_test, {key, type, term, string, term2, term3}).

%%
%% Exported Functions
%%
-export([]).
-compile(export_all).

%%
%% API Functions
%%
insert_bash_data() ->
    TestRecord = [#mysql_test{key = Index1, type = Index2, term = {1,2}, string = "",
                              term2 = [1,2,3,4], term3 = now()}
                 ||Index1<-lists:seq(1, 213), Index2<-lists:seq(1, 213)],
    module_mysql_test:insert(TestRecord).


%%
%% Local Functions
%%

