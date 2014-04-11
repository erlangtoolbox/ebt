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
%% @doc Compile NIF/Port
%%
%% == Configuration ==
%% <ul>
%% <li>nifname - name of the library</li>
%% <li>cc - compiler</li>
%% <li>files - files to compile</li>
%% </ul>
%%
%% == Example ==
%% <pre>
%% {cc, [
%%     {nifname1, [
%%          {linux, [
%%              {cc, "gcc"},
%%              {files, [
%%                   {include, ["c_src/*.c", "c_src/*.cc", "c_src/*.cpp"]},
%%                   {exclude, []}
%%              ]},
%%              {cflags, ""},
%%              {ldflags, ""}
%%          ]}
%%     ]}
%%     {nifname2, []}
%% ]}
%% </pre>
-module(ebt_task_cc).
-author("Volodymyr Kyrychenko <vladimirk.kirichenko@gmail.com>").

-compile({parse_transform, do}).

-export([perform/3]).

perform(Target, Dir, Config) ->
    {_, Os} = os:type(),
    do([error_m ||
        io:format("os is ~s~n", [Os]),
        xl_lists:eforeach(fun({SoName, Cfg}) ->
            OsConfig = xl_lists:kvfind(Os, Cfg, []),
            CC = xl_lists:kvfind(cc, OsConfig, "gcc"),
            Sources = ebt_config:files(OsConfig, [], ["c_src/*.c", "c_src/*.cc", "c_src/*.cpp"]),
            Includes = "-I" ++ hd(filelib:wildcard(code:lib_dir() ++ "/erl_interface-*/include"))
                ++ " -I" ++ hd(filelib:wildcard(code:root_dir() ++ "/erts-*/include")),
            CFlags = "-g -Wall -fPIC " ++ xl_lists:kvfind(cflags, OsConfig, ""),
            LDFlags = "-shared -L" ++ hd(filelib:wildcard(code:lib_dir() ++ "/erl_interface-*/lib"))
                ++ " -lei -lerl_interface " ++ xl_lists:kvfind(ldflags, OsConfig, ""),
            do([error_m ||
                NativeOut <- ebt_config:app_outdir(native, Dir, Config),
                xl_lists:eforeach(fun(File) ->
                    Name = filename:basename(filename:rootname(File)),
                    Command = xl_string:format("~s ~s ~s -c ~s -o ~s/~s.o", [CC, CFlags, Includes, File, NativeOut, Name]),
                    io:format("~s~n", [Command]),
                    xl_shell:command(Command)
                end, Sources),
                SoOut <- ebt_config:output_dir({outdir, production, "priv/" ++
                    xl_lists:kvfind(libdir, OsConfig, "lib")}, Dir, Config),
                xl_file:mkdirs(SoOut),
                Command <- return(xl_string:format("~s ~s -o ~s/~s.so ~s/*.o", [CC, LDFlags, SoOut, SoName, NativeOut])),
                io:format("~s~n", [Command]),
                xl_shell:command(Command)
            ])
        end, ebt_config:value(Target, Config, [])),
        return(Config)
    ]).

