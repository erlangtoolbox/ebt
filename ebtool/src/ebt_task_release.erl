%%  The MIT License (MIT)
%%
%%  Copyright (c) 2014 Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>
%%
%%  Permission is hereby granted, free of charge, to any person obtaining a copy of
%%  this software and associated documentation files (the "Software"), to deal in
%%  the Software without restriction, including without limitation the rights to
%%  use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
%%  the Software, and to permit persons to whom the Software is furnished to do so,
%%  subject to the following conditions:
%%
%%  The above copyright notice and this permission notice shall be included in all
%%  copies or substantial portions of the Software.
%%
%%  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
%%  FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
%%  COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
%%  IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%%  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

-module(ebt_task_release).

-include_lib("kernel/include/file.hrl").

-compile({parse_transform, do}).

-export([perform/3]).

perform(Target, Dir, Config) ->
    do([error_m ||
        RelConfigFile <- ebt_config:find_value(Target, Config, config),
        [RelConfig] <- xl_file:read_terms(RelConfigFile),
        RelConfigUpdated <- return(update_release_config(RelConfig, Config)),
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
        pack(Target, Config),
        return(Config)
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
    {sys, lists:map(fun
        ({rel, Name, _, Apps}) -> {rel, Name, ebt_config:version(Config), Apps};
        (X) -> X
    end, Opts)}.
