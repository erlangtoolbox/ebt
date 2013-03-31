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
-module(ebt_config).

-compile({parse_transform, ebt__do}).

-type(config() :: ebt__xl_lists:kvlist_at()).
-export_types([config/0, defaults/0]).

-export([read/2, value/3, value/4, find_value/2, find_value/3,
    outdir/1, outdir/3, version/1, appname_full/2, appname/1, app_outdir/3,
    outdir/2, build_number/1, info_outdir/2]).

-spec(read(file:name(), config()) -> ebt__error_m:monad(config())).
read(Filename, Defaults) ->
    case ebt__xl_file:exists(Filename) of
        {ok, true} ->
            ebt__do([ebt__error_m ||
                Config <- ebt__xl_file:read_terms(Filename),
                return(ebt__xl_lists:keyreplace_or_add(1, Defaults, Config))
            ]);
        {ok, false} -> {ok, Defaults};
        E -> E
    end.

-spec(find_value(atom(), config()) -> ebt__error_m:monad(any())).
find_value(Key, Config) ->
    ebt__option_m:to_ebt__error_m(ebt__xl_lists:kvfind(Key, Config),
            ebt__xl_string:format("~p not found", [Key])).

-spec(find_value(atom(), config(), atom()) -> ebt__error_m:monad(any())).
find_value(Key, Config, InnerKey) ->
    Error = {error, ebt__xl_string:format("~p/~p not found", [Key, InnerKey])},
    case ebt__xl_lists:kvfind(Key, Config) of
        {ok, V} ->
            case ebt__xl_lists:kvfind(InnerKey, V) of
                undefined -> Error;
                X -> X
            end;
        undefined -> Error
    end.

-spec(value(atom(), config(), any()) -> any()).
value(Key, Config, Default) -> ebt__xl_lists:kvfind(Key, Config, Default).

-spec(value(atom(), config(), atom(), any()) -> any()).
value(Key, Config, InnerKey, Default) ->
    case ebt__xl_lists:kvfind(Key, Config) of
        {ok, V} -> ebt__xl_lists:kvfind(InnerKey, V, Default);
        undefined -> Default
    end.

-spec(outdir(config()) -> ebt__error_m:monad(string())).
outdir(Config) ->
    ebt__do([ebt__error_m ||
        Dir <- ebt_config:find_value(outdir, Config),
        ebt__xl_file:mkdirs(Dir),
        return(Dir)
    ]).

-spec(outdir(atom(), config()) -> ebt__error_m:monad(string())).
outdir(Kind, Config) ->
    ebt__do([ebt__error_m ||
        Parent <- outdir(Config),
        Dir <- return(filename:absname(ebt__xl_string:join([Parent, Kind], "/"))),
        ebt__xl_file:mkdirs(Dir),
        return(Dir)
    ]).

-spec(outdir(atom(), config(), string()) -> ebt__error_m:monad(string())).
outdir(Kind, Config, Suffix) ->
    ebt__do([ebt__error_m ||
        Parent <- outdir(Kind, Config),
        Dir <- return(ebt__xl_string:join([Parent, Suffix], "/")),
        ebt__xl_file:mkdirs(Dir),
        return(Dir)
    ]).

-spec(app_outdir(atom(), file:name(), config()) -> ebt__error_m:monad(string())).
app_outdir(Kind, Dir, Config) ->
    ebt__do([ebt__error_m ||
        App <- appname_full(Dir, Config),
        outdir(Kind, Config, App)
    ]).

-spec(info_outdir(file:name(), config()) -> ebt__error_m:monad(string())).
info_outdir(Dir, Config) ->
    ebt__do([ebt__error_m ||
        AppProdDir <- app_outdir(production, Dir, Config),
        InfoDir <- return(ebt__xl_string:join([AppProdDir, ".ebt-info"], "/")),
        ebt__xl_file:mkdirs(InfoDir),
        return(InfoDir)
    ]).

-spec(version(config()) -> ebt__error_m:monad(string())).
version(Config) ->
    case ebt__xl_lists:kvfind(version, Config) of
        {ok, {shell, Cmd}} -> ebt__xl_shell:command(Cmd);
        _ -> {ok, "0.0.1"}
    end.

-spec(build_number(config()) -> ebt__error_m:monad(string())).
build_number(Config) ->
    case ebt__xl_lists:kvfind(build, Config) of
        {ok, {shell, Cmd}} -> ebt__xl_shell:command(Cmd);
        _ -> {ok, "0"}
    end.

-spec(appname_full(file:name(), config()) -> ebt__error_m:monad(string())).
appname_full(Dir, Config) ->
    ebt__do([ebt__error_m ||
        {_, Name, _} <- ebt_applib:load(Dir ++ "/src"),
        Version <- ebt_config:version(Config),
        return(ebt__xl_string:join([Name, Version], "-"))
    ]).

-spec(appname(file:name()) -> ebt__error_m:monad(string())).
appname(Dir) ->
    ebt__do([ebt__error_m ||
        {_, Name, _} <- ebt_applib:load(Dir ++ "/src"),
        return(atom_to_list(Name))
    ]).

