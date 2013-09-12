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

-compile({parse_transform, ebt__do}).
-behaviour(ebt_task).

-export([perform/3]).

perform(Target, Dir, Config) ->
    Libraries = ebt_config:libraries(Config),
    lists:foreach(fun(L) -> io:format("include ~s~n", [L]) end, Libraries),
    ebt__do([ebt__error_m ||
        AppProdDir <- ebt_config:app_outdir(production, Dir, Config),
        LibBeams <- ebt__xl_lists:eflatmap(fun(L) ->
            ebt__xl_file:read_files([L ++ "/ebin/*"])
        end, Libraries),
        LibPrivs <- ebt__xl_lists:eflatmap(fun(L) ->
            ebt__xl_file:read_files([L ++ "/priv/*"], {base, L})
        end, Libraries),
        Files <- ebt__xl_file:read_files([AppProdDir ++ "/ebin/*"]),
        Scripts <- ebt_config:find_value(Target, Config),
        ebt__xl_lists:eforeach(fun(ScriptConfig) ->
            create_escript(ScriptConfig, AppProdDir, Dir, LibBeams ++ LibPrivs ++ Files)
        end, Scripts)
    ]).

create_escript({Name, Params, Resources, {priv_link, Target}}, AppProdDir, Dir, Files) ->
    ebt__do([error_m ||
        ebt__xl_file:mkdirs(ebt__xl_string:join([AppProdDir, "/bin"])),
        ebt__xl_file:make_symlink(Target, ebt__xl_string:join([AppProdDir, "/bin/priv"])),
        Beams <- ebt__xl_lists:emap(fun(F) ->
            case ebt__xl_escript:read_file(F) of
                {ok, Bin} -> {ok, {F, Bin}};
                E -> E
            end
        end, [
            "ebt__xl_escript.beam",
            "ebt__xl_lists.beam",
            "ebt__xl_file.beam",
            "ebt__xl_io.beam",
            "ebt__error_m.beam"
        ]),
        create_escript({Name, Params, Resources}, AppProdDir, Dir, Beams ++ Files)
    ]);
create_escript({Name, Params, Resources}, AppProdDir, Dir, Files) ->
    Path = ebt__xl_string:join([AppProdDir, "/bin/", Name]),
    ebt__do([ebt__error_m ||
        ResourceFiles <- ebt__xl_file:read_files(Resources, {base, Dir}),
        {"memory", Zip} <- zip:create("memory", Files ++ ResourceFiles, [memory]),
        ebt__xl_file:ensure_dir(Path),
        escript:create(Path, [
            {shebang, default},
            {comment, default},
            {emu_args, Params},
            {archive, Zip}
        ]),
        #file_info{mode = Mode} <- ebt__xl_file:read_file_info(Path),
        ebt__xl_file:change_mode(Path, Mode bor 8#00100),
        io:format("create ~s~n", [Path])
    ]).
