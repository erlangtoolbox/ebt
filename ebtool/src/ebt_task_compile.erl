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
%% @doc Compile erlang sources and tests
%%
%% == Configuration ==
%% <ul>
%% <li>sources - list of lists of wildcards</li>
%% <li>tests - list of lists of wildcards</li>
%% <li>resources - files to copy to ebin</li>
%% <li>flags - compilation flags</li>
%% </ul>
%%
%% == Example ==
%% <pre>
%% {compile, [
%%     {sources, [["src/*.erl"]]},
%%     {tests, [["src/*.erl"]]},
%%     {resources, ["src/*.json"]},
%%     {flags, [warnings_as_errors, debug_info]}
%% ]}
%% </pre>
-module(ebt_task_compile).

-compile({parse_transform, do}).

-export([perform/3]).

perform(Target, Dir, Config) ->
    SrcDir = Dir ++ "/src",
    TestDir = Dir ++ "/test",
    do([error_m ||
        AppProdDir <- ebt_config:app_outdir(production, Dir, Config),
        EbinProdDir <- return(AppProdDir ++ "/ebin"),
        compile(Target, SrcDir, EbinProdDir, Config, {sources, [["src/*.erl"]]}),
        AppSpec <- et_code_appdesc:load(SrcDir),
        update_app(AppSpec, EbinProdDir, Config),
        xl_file:copy_if_exists(Dir ++ "/src", AppProdDir),
        xl_file:copy_if_exists(Dir ++ "/include", AppProdDir),
        xl_file:copy_if_exists(Dir ++ "/priv", AppProdDir),
        xl_file:copy_if_exists(Dir ++ "/bin", AppProdDir),
        xl_file:copy_filtered(SrcDir, ebt_config:value(Target, Config, resources, []), EbinProdDir),
        AppTestDir <- ebt_config:app_outdir(test, Dir, Config),
        EbinTestDir <- return(AppTestDir ++ "/ebin"),
        HasTests <- xl_file:exists(Dir ++ "/test"),
        case HasTests of
            true ->
                do([error_m ||
                    compile(Target, TestDir, EbinTestDir, Config, {tests, [["test/*.erl"]]}),
                    xl_file:copy_filtered(TestDir,
                        ebt_config:value(Target, Config, resources, []), EbinTestDir)
                ]);
            false -> ok
        end,
        return(Config)
    ]).

-spec(update_app(application:application_spec(), file:name(), ebt_config:config()) -> error_m:monad(ok)).
update_app(AppSpec = {_, App, _}, EbinProdDir, Config) ->
    Filename = xl_string:join([EbinProdDir, "/", App, ".app"], ""),
    Modules = [list_to_atom(filename:basename(F, ".beam")) || F <- filelib:wildcard(EbinProdDir ++ "/*.beam")],
    AppDescriptor = et_code_appdesc:update(AppSpec, [
        {modules, Modules},
        {vsn, ebt_config:version(Config)}
    ]),
    xl_file:write_term(Filename, AppDescriptor).

-spec(compile(atom(), file:name(), file:name(), ebt_config:config(), {atom(), [[string()]]}) -> error_m:monad(ok)).
compile(Target, SrcDir, OutDir, Config, {Key, Defaults}) when is_atom(Target) ->
    do([error_m ||
        io:format("compiling ~s to ~s~n", [SrcDir, OutDir]),
        Includes <- return([{i, Lib} || Lib <- ebt_config:value(libraries, Config, [])]),
        Flags <- return(ebt_config:value(Target, Config, flags, []) ++ Includes),
        xl_file:mkdirs(OutDir),
        xl_lists:eforeach(fun(Files) ->
            compile_files(xl_file:wildcards(Files), Flags, OutDir, Config)
        end, ebt_config:value(Target, Config, Key, Defaults))
    ]).

compile_files(Files, Flags, OutDir, Config) ->
    Options = [{outdir, OutDir}, {i, "./include"}, report | Flags],
    do([error_m ||
        ebtool:load_libraries(Config),
        lists:foldl(fun(File, Status) ->
            io:format("compile ~s~n", [File]),
            case compile:file(File, Options) of
                {ok, _} -> Status;
                error -> {error, "...compilation failed!"}
            end
        end, ok, Files)
    ]).

