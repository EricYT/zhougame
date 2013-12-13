%% base macro
-define(NULL, undefined).


-define(IF(C, V1, V2), case (C) of true -> (V1); false -> V2 end).

-define(TRY_CATCH(CHUNKS), begin try CHUNKS catch E:R -> debug:info("Error ~p ~p", [E, R]) end end).