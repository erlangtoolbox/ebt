%%  Copyright (c) 2012-2013
%%  StrikeAd LLC http://www.strikead.com
%%
%%  All rights reserved.
%%
%%  Redistribution and use in source and binary forms, with or without
%%  modification, are permitted provided that the following conditions are met:
%%
%%      Redistributions of source code must retain the above copyright
%%  notice, this list of conditions and the following disclaimer.
%%      Redistributions in binary form must reproduce the above copyright
%%  notice, this list of conditions and the following disclaimer in the
%%  documentation and/or other materials provided with the distribution.
%%      Neither the name of the StrikeAd LLC nor the names of its
%%  contributors may be used to endorse or promote products derived from
%%  this software without specific prior written permission.
%%
%%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
%%  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
%%  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
%%  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%%  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
%%  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
%%  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-module(ebt_task_release).

-include_lib("kernel/include/file.hrl").

-compile({parse_transform, do}).
-behaviour(ebt_task).

-export([perform/3]).

perform(Target, Dir, Config) ->
    do([error_m ||
        RelConfigFile <- ebt_config:find_value(Target, Config, config),
        [RelConfig] <- xl_file:read_terms(RelConfigFile),
        RelConfigUpdated <- update_release_config(RelConfig, Config),
        Name <- ebt_config:find_value(Target, Config, name),
        ReleaseDir <- ebt_config:outdir(releases, Config, Name),
        io:format("release ~p to ~s~n", [Name, ReleaseDir]),
        RelTool <- reltool:start_server([{config, RelConfigUpdated}]),
        try
            reltool:create_target(RelTool, ReleaseDir)
        after
            reltool:stop(RelTool)
        end,
        xl_file:copy_filtered(Dir, ebt_config:value(Target, Config, resources, []), ReleaseDir),
        generate_runners(RelConfigUpdated, ReleaseDir),
        pack(Target, Config)
    ]).

generate_runners({sys, L}, ReleaseDir) ->
    do([error_m ||
        RunnerTemplate <- xl_escript:read_file("priv/release/run"),
        xl_lists:eforeach(fun({rel, Name, _, _}) ->
            Path = xl_string:join([ReleaseDir, "bin", Name], "/"),
            do([error_m ||
                Runner <- return(xl_string:substitute(binary_to_list(RunnerTemplate), [
                    {'APPNAME', Name}
                ], {$@, $@})),
                xl_file:write_file(Path, Runner),
                #file_info{mode = Mode} <- xl_file:read_file_info(Path),
                xl_file:change_mode(Path, Mode bor 8#00100)
            ])
        end, lists:filter(fun(X) -> element(1, X) == rel end, L))
    ]).


pack(Target, Config) ->
    do([error_m ||
        Name <- ebt_config:find_value(Target, Config, name),
        DestDir <- ebt_config:outdir(releases, Config),
        DistDir <- ebt_config:outdir(dist, Config),
        ebt_cmdlib:exec(xl_string:join(["tar -czf ", DistDir, "/", Name, ".tar.gz ", Name]), DestDir)
    ]).

update_release_config({sys, Opts}, Config) ->
    do([error_m ||
        Version <- ebt_config:version(Config),
        return({sys, lists:map(fun
            ({rel, Name, _, Apps}) -> {rel, Name, Version, Apps};
            (X) -> X
        end, Opts)})
    ]).
