

%%
%% A define file that contains the records for protobuffs
%%

%% The message record
-record(message, {message_name, columns}).

%%
-record(enum, {id, message_head}).