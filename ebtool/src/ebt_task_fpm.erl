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
%%
%%  All rights reserved.
%%
%%  Redistribution and use in source and binary forms, with or without
%%  modification, are permitted provided that the following conditions are met:
%%
%%

%% @doc Compile NIF/Port
%%
%% == Example ==
%% <pre>
%% {fpm, [
%%     {package_name, [
%%         {type, rpm},
%%         {files, [{include, ["target/scala*/com-strikead-navarro*-one-jar.jar", "bin", "conf"]}]},
%%         {params, [
%%             {prefix, "/opt"}
%%              .....
%%         ]}
%%     ]}
%% ]}.
%% </pre>
%% @end
-module(ebt_task_fpm).
-author("volodymyr.kyrychenko@strikead.com").

-compile({parse_transform, do}).

-include_lib("kernel/include/file.hrl").

-export([perform/3]).

perform(Target, Dir, Config) ->
    do([error_m ||
        xl_lists:eforeach(fun({Name, Params}) ->
            P = lists:foldl(fun({Key, Value}, Acc) ->
                Acc ++ xl_string:format(" --~s=~p", [Key, Value])
            end, "", [{version, ebt_config:version(Config)} | xl_lists:kvfind(params, Params, [])]),
            do([error_m ||
                OutputDir <- ebt_config:output_dir({outdir, "fpm"}, Dir, Config),
                DistDir <- ebt_config:output_dir({outdir, "dist"}, Dir, Config),
                xl_lists:eforeach(fun(F) ->
                    xl_file:copy_if_exists(F, xl_string:join([OutputDir, "/", Name]))
                end, ebt_config:files(Params, files, [], [])),
                ebt_cmdlib:exec({"fpm -f -s dir -t ~s -n ~s -p ~s ~s ~s", [xl_lists:kvfind(type, Params, rpm), Name, DistDir, P, Name]}, OutputDir)
            ])
        end, ebt_config:value(Target, Config, [])),
        return(Config)
    ]).


