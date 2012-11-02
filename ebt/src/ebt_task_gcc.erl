-module(ebt_task_gcc).

-compile({parse_transform, do}).
-behaviour(ebt_task).

-export([perform/3]).

perform(Target, Dir, Config) ->
    case ebt_config:find_value(Target, Config) of
        {ok, _} ->
            CC = ebt_config:value(Target, Config, cc, "gcc"),
            Sources = filelib:wildcard(filename:join([Dir, ebt_config:value(Target, Config, sources, "c_src"), "*.c"])),
            Includes = "-I" ++ hd(filelib:wildcard(code:lib_dir() ++ "/erl_interface-*/include"))
                ++ " -I" ++ hd(filelib:wildcard(code:root_dir() ++ "/erts-*/include")),
            CFlags = "-g -Wall -fPIC " ++ ebt_config:value(Target, Config, cflags, ""),
            LDFlags = "-shared -L" ++ hd(filelib:wildcard(code:lib_dir() ++ "/erl_interface-*/lib")) ++ " -lei -lerl_interface " ++ ebt_config:value(Target, Config, ldflags, ""),
            do([error_m ||
                NativeOut <- ebt_config:outdir(native, Config),
                ebt_xl_lists:eforeach(fun(File) ->
                    Name = filename:basename(File, ".c"),
                    Command = ebt_xl_string:format("~s ~s ~s -c ~s -o ~s/~s.o", [CC, CFlags, Includes, File, NativeOut, Name]),
                    io:format("~s~n", [Command]),
                    ebt_xl_shell:command(Command)
                end, Sources),
                SoName <- ebt_config:find_value(Target, Config, name),
                SoOut <- ebt_config:app_outdir(production, Dir, Config),
                ebt_xl_file:mkdirs(SoOut ++ "/priv"),
                Command <- return(ebt_xl_string:format("~s ~s -o ~s/priv/~s.so ~s/*.o", [CC, LDFlags, SoOut, SoName, NativeOut])),
                io:format("~s~n", [Command]),
                ebt_xl_shell:command(Command)
            ]);
        _ -> ok
    end.

