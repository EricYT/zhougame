
%% The mysql or proto module define.
-record(module_define, {module_name, columns, primary_key, index, engine}).

%%
-record(columns_define, {col_name, type, auto_increment, is_null, default, description}).