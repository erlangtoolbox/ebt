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

-module(ebt_task_protoc).

-compile({parse_transform, do}).

-export([perform/3]).

perform(_Target, Dir, Config) ->
    Sources = filelib:wildcard(Dir ++ "/src/*.proto"),
    IncludeDir = Dir ++ "/include",
    do([error_m ||
        OutDir <- ebt_config:app_outdir(production, Dir, Config),
        EbinDir <- return(OutDir ++ "/ebin"),
        xl_file:mkdirs(EbinDir),
        xl_lists:eforeach(fun(File) ->
            do([error_m ||
                xl_file:mkdirs(IncludeDir),
                protobuffs_compile:scan_file(File, [
                    {output_ebin_dir, EbinDir},
                    {output_include_dir, IncludeDir}
                ])
            ])
        end, Sources),
        return(Config)
    ]).
