-module(ebt_protoc).

-compile({parse_transform, do}).

-behaviour(ebt_task).

-export([perform/2]).

perform(Dir, Config) ->
    Sources = filelib:wildcard(Dir ++ "/src/*.proto"),
    do([error_m ||
        OutDir <- ebt_config:app_outdir(production, Dir, Config),
        EbinDir <- return(OutDir ++ "/ebin"),
        ebt_xl_file:mkdirs(EbinDir),
        ebt_xl_lists:eforeach(fun(File) ->
            protobuffs_compile:scan_file(File, [
                {output_ebin_dir, EbinDir},
                {output_include_dir, Dir ++ "/include"}
            ])
        end, Sources)
    ]).
