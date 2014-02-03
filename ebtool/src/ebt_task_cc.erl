%%  Copyright (c) 2012-2013 StrikeAd LLC http://www.strikead.com
%%  Copyright (c) 2012-2014 Vladimir Kirichenko vladimir.kirichenko@gmail.com
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
%%      Neither the name of the EBT nor the names of its
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
                    xl_lists:kvfind(lib_dir, OsConfig, "lib")}, Dir, Config),
                xl_file:mkdirs(SoOut),
                Command <- return(xl_string:format("~s ~s -o ~s/~s.so ~s/*.o", [CC, LDFlags, SoOut, SoName, NativeOut])),
                io:format("~s~n", [Command]),
                xl_shell:command(Command)
            ])
        end, ebt_config:value(Target, Config, [])),
        return(Config)
    ]).

