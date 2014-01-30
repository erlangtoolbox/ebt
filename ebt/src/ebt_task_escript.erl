%%  Copyright (c) 2012-2013 StrikeAd LLC http://www.strikead.com
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

%% @doc Generating escript
%%
%% == Configuration ==
%% {scriptname, emu_args, resources} - name of executable, emulator arguments and resources to be included
%%
%% == Example ==
%% <pre>
%% {escript, [
%%     {scriptname1, [
%%         {emu_args, "-noshell -noinput +d"},
%%         {comment, "-noshell -noinput +d"},
%%         {shebang, "-noshell -noinput +d"},
%%         {apps, [{include, ["./lib/*"]}]} % fileset
%%     ]},
%%     {scriptname2, [
%%         {emu_args, "-noshell -noinput +d"}
%%     ]},
%% ]}
%% </pre>
-module(ebt_task_escript).

-include_lib("kernel/include/file.hrl").

-compile({parse_transform, ebt__do}).

-behaviour(ebt_task).

-export([perform/3]).

perform(Target, Dir, Config) ->
    ebt__do([ebt__error_m ||
        AppProdDir <- ebt_config:app_outdir(production, Dir, Config),
        Files <- ebt__xl_file:read_files([AppProdDir ++ "/ebin/*"]),
        Priv <- ebt__xl_file:read_files([AppProdDir ++ "/priv/*"], {base, AppProdDir}),
        Scripts <- ebt_config:find_value(Target, Config),
        ebt__xl_lists:eforeach(fun(ScriptConfig) ->
            create_escript(ScriptConfig, AppProdDir, Files ++ Priv)
        end, Scripts),
        return(Config)
    ]).

create_escript({Name, Params}, AppProdDir, Files) ->
    Path = ebt__xl_string:join([AppProdDir, "/bin/", Name]),
    Apps = ebt_config:files(Params, apps, [], []),
    ebt__do([ebt__error_m ||
        AppBeams <- ebt__xl_lists:eflatmap(fun(A) ->
            ebt__xl_file:read_files([A ++ "/ebin/*"])
        end, Apps),
        LibPrivs <- ebt__xl_lists:eflatmap(fun(A) ->
            ebt__xl_file:read_files([A ++ "/priv/*"], {base, A})
        end, Apps),
        {"memory", Zip} <- zip:create("memory", Files ++ AppBeams ++ LibPrivs, [memory]),
        ebt__xl_file:ensure_dir(Path),
        escript:create(Path, [
            {shebang, ebt__xl_lists:kvfind(shebang, Params, default)},
            {comment, ebt__xl_lists:kvfind(comment, Params, default)},
            {emu_args, ebt__xl_lists:kvfind(emu_args, Params, "")},
            {archive, Zip}
        ]),
        #file_info{mode = Mode} <- ebt__xl_file:read_file_info(Path),
        ebt__xl_file:change_mode(Path, Mode bor 8#00100),
        io:format("create ~s~n", [Path])
    ]).
