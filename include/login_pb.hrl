
%% server message proto

-record(login_c2s, {msgid=1, id, name}).
-record(login_s2c, {msgid=2, res}).