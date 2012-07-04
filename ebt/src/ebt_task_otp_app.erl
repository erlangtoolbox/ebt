-module(ebt_task_otp_app).

-behaviour(ebt_task).

-export([perform/2]).

perform(Dir, _Config) ->
    ebt:report("otp-app...~s", [Dir]).
