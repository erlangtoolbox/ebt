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


