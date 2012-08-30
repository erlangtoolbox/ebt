-module(ebt_task_protoc).

-compile({parse_transform, do}).

-behaviour(ebt_task).

-export([perform/3]).

perform(_Target, Dir, Config) ->
    Sources = filelib:wildcard(Dir ++ "/src/*.proto"),
    IncludeDir = Dir ++ "/include",
    do([error_m ||
        OutDir <- ebt_config:app_outdir(production, Dir, Config),
        EbinDir <- return(OutDir ++ "/ebin"),
        ebt_xl_file:mkdirs(EbinDir),
        ebt_xl_lists:eforeach(fun(File) ->
            do([error_m ||
                ebt_xl_file:mkdirs(IncludeDir),
                protobuffs_compile:scan_file(File, [
                    {output_ebin_dir, EbinDir},
                    {output_include_dir, IncludeDir}
                ])
            ])
        end, Sources)
    ]).
