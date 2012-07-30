-spec start/2 :: (StartType :: normal | {takeover, node()} | {failover, node()}, StartArgs :: term()) ->
    {ok, pid()} | {ok, pid(), State :: term()} | {error, Reason :: term()}.

-spec stop/1 :: (State :: term()) -> term().

