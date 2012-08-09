-module(ebt_strikead_autoresource).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{auto_open,1},
     {auto_close,1}];

behaviour_info(_) -> undefined.

