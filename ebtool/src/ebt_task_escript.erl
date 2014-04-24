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
%% @doc Generating escript
%%
%% == Configuration ==
%% {scriptname, emu_args, resources} - name of executable, emulator arguments and resources to be included
%%
%% == Example ==
%% <pre>
%% {escript, [
%%     {scriptname1, [
%%         {emu_args, "-noshell -noinput +d"},
%%         {comment, "-noshell -noinput +d"},
%%         {shebang, "-noshell -noinput +d"},
%%         {apps, [{include, ["./lib/*"]}]} % fileset
%%     ]},
%%     {scriptname2, [
%%         {emu_args, "-noshell -noinput +d"}
%%     ]},
%% ]}
%% </pre>
-module(ebt_task_escript).

-include_lib("kernel/include/file.hrl").

-compile({parse_transform, do}).

-export([perform/3]).

perform(Target, Dir, Config) ->
    do([error_m ||
        AppProdDir <- ebt_config:app_outdir(production, Dir, Config),
        Scripts <- ebt_config:find_value(Target, Config),
        xl_lists:eforeach(fun(ScriptConfig) ->
            create_escript(ScriptConfig, AppProdDir, [])
        end, Scripts),
        return(Config)
    ]).

create_escript({Name, Params}, AppProdDir, Files) ->
    Path = xl_string:join([AppProdDir, "/bin/", Name]),
    Apps = ebt_config:files(Params, apps, [], []),
    do([error_m ||
        AppBeams <- xl_lists:eflatmap(fun(A) ->
            xl_file:read_files([A ++ "/ebin/*"])
        end, Apps),
        LibPrivs <- xl_lists:eflatmap(fun(A) ->
            xl_file:read_files([A ++ "/priv/*"], {base, A})
        end, Apps),
        {"memory", Zip} <- zip:create("memory", Files ++ AppBeams ++ LibPrivs, [memory]),
        xl_file:ensure_dir(Path),
        escript:create(Path, [
            {shebang, xl_lists:kvfind(shebang, Params, default)},
            {comment, xl_lists:kvfind(comment, Params, default)},
            {emu_args, xl_lists:kvfind(emu_args, Params, "")},
            {archive, Zip}
        ]),
        #file_info{mode = Mode} <- xl_file:read_file_info(Path),
        xl_file:change_mode(Path, Mode bor 8#00100),
        io:format("create ~s~n", [Path])
    ]).
