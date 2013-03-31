-module(ebt_task_cc).

-compile({parse_transform, ebt__do}).
-behaviour(ebt_task).

-export([perform/3]).

perform(Target, Dir, Config) ->
    case ebt_config:find_value(Target, Config) of
        {ok, _} ->
            {_, Os} = os:type(),
            io:format("os is ~s~n", [Os]),
            OsConfig = ebt_config:value(Target, Config, Os, []),
            CSourceDir = ebt_config:value(Target, Config, sources, "c_src"),
            CC = ebt__xl_lists:kvfind(cc, OsConfig, "gcc"),
            Sources = lists:append([filelib:wildcard(filename:join([Dir, CSourceDir, WC])) || WC <- ["*.c", "*.cc", "*.cpp"]]),
            Includes = "-I" ++ hd(filelib:wildcard(code:lib_dir() ++ "/erl_interface-*/include"))
                ++ " -I" ++ hd(filelib:wildcard(code:root_dir() ++ "/erts-*/include")),
            CFlags = "-g -Wall -fPIC " ++ ebt__xl_lists:kvfind(cflags, OsConfig, ""),
            LDFlags = "-shared -L" ++ hd(filelib:wildcard(code:lib_dir() ++ "/erl_interface-*/lib")) ++ " -lei -lerl_interface " ++ ebt__xl_lists:kvfind(ldflags, Config, ""),
            ebt__do([ebt__error_m ||
                NativeOut <- ebt_config:outdir(native, Config),
                ebt__xl_lists:eforeach(fun(File) ->
                    Name = filename:basename(filename:rootname(File)),
                    Command = ebt__xl_string:format("~s ~s ~s -c ~s -o ~s/~s.o", [CC, CFlags, Includes, File, NativeOut, Name]),
                    io:format("~s~n", [Command]),
                    ebt__xl_shell:command(Command)
                end, Sources),
                SoName <- ebt_config:find_value(Target, Config, name),
                SoOut <- ebt_config:app_outdir(production, Dir, Config),
                ebt__xl_file:mkdirs(SoOut ++ "/priv"),
                Command <- return(ebt__xl_string:format("~s ~s -o ~s/priv/~s.so ~s/*.o", [CC, LDFlags, SoOut, SoName, NativeOut])),
                io:format("~s~n", [Command]),
                ebt__xl_shell:command(Command)
            ]);
        _ -> ok
    end.

