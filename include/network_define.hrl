
-define(INIT_PACKET, {packet, 0}).

-define(TCP_OPTIONS, [binary, ?INIT_PACKET, {reuseaddr, true}, {keepalive, true}, {backlog, 256}, {active, false}]).

