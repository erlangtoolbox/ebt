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
-module(ebt_task_compile).

-compile({parse_transform, ebt__do}).

-behaviour(ebt_task).

-export([perform/3]).

-spec(perform(atom(), file:name(), ebt_config:config()) -> ebt__error_m:monad(ok)).
perform(Target, Dir, Config) ->
    SrcDir = Dir ++ "/src",
    TestDir = Dir ++ "/test",
    ebt__do([ebt__error_m ||
        AppProdDir <- ebt_config:app_outdir(production, Dir, Config),
        EbinProdDir <- return(AppProdDir ++ "/ebin"),
        compile(Target, SrcDir, EbinProdDir, Config),
        AppSpec <- ebt_applib:load(SrcDir),
        update_app(AppSpec, EbinProdDir, Config),
        ebt__xl_file:copy_if_exists(Dir ++ "/src", AppProdDir),
        ebt__xl_file:copy_if_exists(Dir ++ "/include", AppProdDir),
        ebt__xl_file:copy_if_exists(Dir ++ "/priv", AppProdDir),
        ebt__xl_file:copy_if_exists(Dir ++ "/bin", AppProdDir),
        ebt__xl_file:copy_filtered(SrcDir, ebt_config:value(Target, Config, resources, []), EbinProdDir),
        AppTestDir <- ebt_config:app_outdir(test, Dir, Config),
        EbinTestDir <- return(AppTestDir ++ "/ebin"),
        HasTests <- ebt__xl_file:exists(Dir ++ "/test"),
        case HasTests of
            true ->
                ebt__do([ebt__error_m ||
                    compile(Target, TestDir, EbinTestDir, Config),
                    ebt__xl_file:copy_filtered(TestDir,
                        ebt_config:value(Target, Config, resources, []), EbinTestDir)
                ]);
            false -> ok
        end
    ]).

-spec(update_app(application:application_spec(), file:name(), ebt_config:config()) -> ebt__error_m:monad(ok)).
update_app(AppSpec = {_, App, _}, EbinProdDir, Config) ->
    Filename = ebt__xl_string:join([EbinProdDir, "/", App, ".app"], ""),
    Modules = [list_to_atom(filename:basename(F, ".beam")) || F <- filelib:wildcard(EbinProdDir ++ "/*.beam")],
    ebt__do([ebt__error_m ||
        Version <- ebt_config:version(Config),
        ebt__xl_file:write_term(Filename, ebt_applib:update(AppSpec, [{modules, Modules}, {vsn, Version}]))
    ]).

-spec(compile(atom(), file:name(), file:name(), ebt_config:config()) -> ebt__error_m:monad(ok)).
compile(Target, SrcDir, OutDir, Config) when is_atom(Target) ->
    ebt__do([ebt__error_m ||
        io:format("compiling ~s to ~s~n", [SrcDir, OutDir]),
        Includes <- return([{i, Lib} || Lib <- ebt_config:value(libraries, Config, [])]),
        Flags <- return(ebt_config:value(Target, Config, flags, []) ++ Includes),
        ebt__xl_file:mkdirs(OutDir),
        ebt__xl_lists:eforeach(fun(Files) ->
            compile_files(ebt__xl_file:wildcards(Files), Flags, OutDir, Config)
        end, ebt_config:value(Target, Config, sources, [["src/*.erl"]]))
    ]).

compile_files(Files, Flags, OutDir, Config) ->
    Options = [{outdir, OutDir}, {i, "./include"}, report | Flags],
    ebt__do([ebt__error_m ||
        ebt:load_libraries(Config),
        lists:foldl(fun(File, Status) ->
            io:format("compile ~s~n", [File]),
            case compile:file(File, Options) of
                {ok, _} -> Status;
                error -> {error, "...compilation failed!"}
            end
        end, ok, Files)
    ]).

