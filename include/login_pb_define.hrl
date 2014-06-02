
%% 消息模块的结构体

%% 消息声明
-record(message_heads, {messages}).
-record(head_attr, {msg_name, id, type, mod, comment}).

%% 消息的具体定义
-record(msg_normal, {msg_name, msg_attrs}).               
-record(msg_attr, {field_name, id, base_type, type, comment}).

%% 私有类型定义结构
-record(type_private, {type_name, attrs}).
