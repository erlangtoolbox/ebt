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
-module(ebt_task_cc).

-compile({parse_transform, ebt__do}).
-behaviour(ebt_task).

-export([perform/3]).

perform(Target, Dir, Config) ->
    case ebt_config:find_value(Target, Config) of
        {ok, Natives} ->
            {_, Os} = os:type(),
            io:format("os is ~s~n", [Os]),
            ebt__xl_lists:eforeach(fun({SoName, Cfg}) ->
                OsConfig = ebt__xl_lists:kvfind(Os, Cfg, []),
                CC = ebt__xl_lists:kvfind(cc, OsConfig, "gcc"),
                Sources = ebt_config:files(OsConfig, [], ["c_src/*.c", "c_src/*.cc", "c_src/*.cpp"]),
                Includes = "-I" ++ hd(filelib:wildcard(code:lib_dir() ++ "/erl_interface-*/include"))
                    ++ " -I" ++ hd(filelib:wildcard(code:root_dir() ++ "/erts-*/include")),
                CFlags = "-g -Wall -fPIC " ++ ebt__xl_lists:kvfind(cflags, OsConfig, ""),
                LDFlags = "-shared -L" ++ hd(filelib:wildcard(code:lib_dir() ++ "/erl_interface-*/lib")) ++ " -lei -lerl_interface " ++ ebt__xl_lists:kvfind(ldflags, OsConfig, ""),
                ebt__do([ebt__error_m ||
                    NativeOut <- ebt_config:app_outdir(native, Dir, Config),
                    ebt__xl_lists:eforeach(fun(File) ->
                        Name = filename:basename(filename:rootname(File)),
                        Command = ebt__xl_string:format("~s ~s ~s -c ~s -o ~s/~s.o", [CC, CFlags, Includes, File, NativeOut, Name]),
                        io:format("~s~n", [Command]),
                        ebt__xl_shell:command(Command)
                    end, Sources),
                    SoOut <- ebt_config:app_outdir(production, Dir, Config),
                    ebt__xl_file:mkdirs(SoOut ++ "/priv"),
                    Command <- return(ebt__xl_string:format("~s ~s -o ~s/priv/~s.so ~s/*.o", [CC, LDFlags, SoOut, SoName, NativeOut])),
                    io:format("~s~n", [Command]),
                    ebt__xl_shell:command(Command)
                ])
            end, Natives);
        _ -> ok
    end.
