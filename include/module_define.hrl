
%% The mysql or proto module define.
-record(module_define, {module_name, columns, primary_key, index, engine}).

%%
-record(columns_define, {col_name, type, length, default, is_null, description}).

%% proto
-record(proto_define, {proto_name, columns, key, type}).

-record(attr_define, {col_name, type, description}).