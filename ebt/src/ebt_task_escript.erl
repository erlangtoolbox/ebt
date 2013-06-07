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

%% @doc Generating escript
%%
%% == Configuration ==
%% {scriptname, emu_args, resources} - name of executable, emulator arguments and resources to be included
%%
%% == Example ==
%% <pre>
%% {escript, [
%%     {scriptname1, "-noshell -noinput +d", ["priv/*"]},
%%     {scriptname2, "-noshell -noinput +d", ["priv/*"]},
%% ]}
%% </pre>
-module(ebt_task_escript).

-include_lib("kernel/include/file.hrl").

-compile({parse_transform, do}).
-behaviour(ebt_task).

-export([perform/3]).

perform(Target, Dir, Config) ->
    do([error_m ||
        AppProdDir <- ebt_config:app_outdir(production, Dir, Config),
        Libs <- return(lists:map(fun(L) -> L ++ "/ebin/*" end, ebt_config:libraries(Config))),
        Files <- xl_file:read_files([AppProdDir ++ "/ebin/*" | Libs]),
        Scripts <- ebt_config:find_value(Target, Config),
        xl_lists:eforeach(fun({Name, Params, Resources}) ->
            Path = xl_string:join([AppProdDir, "/bin/", Name]),
            do([error_m ||
                ResourceFiles <- xl_file:read_files(Resources, {base, Dir}),
                {"memory", Zip} <- zip:create("memory", Files ++ ResourceFiles, [memory]),
                xl_file:ensure_dir(Path),
                escript:create(Path, [
                    {shebang, default},
                    {comment, default},
                    {emu_args, Params},
                    {archive, Zip}
                ]),
                #file_info{mode = Mode} <- xl_file:read_file_info(Path),
                xl_file:change_mode(Path, Mode bor 8#00100),
                io:format("created ~s~n", [Path])
            ])
        end, Scripts)
    ]).
