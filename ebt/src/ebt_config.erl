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
-module(ebt_config).

-compile({parse_transform, ebt__do}).

-type(config() :: ebt__xl_lists:kvlist_at()).
-export_types([config/0, defaults/0]).

-export([read/3, value/3, value/4, find_value/2, find_value/3,
    outdir/1, outdir/3, version/1, appname_full/2, appname/1, app_outdir/3,
    outdir/2, info_outdir/2, files/3, files/4, libraries/1, libinfo/1, definition/3, definitions/1]).

-spec(read(file:name(), config(), [{define, atom(), term()}]) -> ebt__error_m:monad(config())).
read(Filename, Defaults, Defines) ->
    case ebt__xl_file:exists(Filename) of
        {ok, true} ->
            ebt__do([ebt__error_m ||
                Config <- ebt__xl_file:read_terms(Filename),
                eval_definitions(lists:keymerge(1, lists:keysort(1, Config), lists:keysort(1, Defaults)), Defines)
            ]);
        {ok, false} -> {ok, Defaults ++ Defines};
        E -> E
    end.


eval_definitions(Config, Defines) ->
    WithDfines = lists:foldl(fun(ND, Cfg) ->
        Key = element(2, ND),
        case ebt__xl_lists:keyfind(Key, 2, Cfg) of
            {ok, E = {define, Key, _}} -> [ND | lists:delete(E, Cfg)];
            _ -> [ND | Cfg]
        end
    end, Config, Defines),
    ebt__xl_lists:emap(fun
        ({define, Key, Value}) ->
            ebt__do([ebt__error_m ||
                V <- evaluate(Value),
                return({define, Key, V})
            ]);
        (X) -> {ok, X}
    end, WithDfines).

evaluate({shell, Cmd}) -> ebt__xl_shell:command(Cmd);
evaluate(X) -> {ok, X}.

-spec(find_value(atom(), config()) -> ebt__error_m:monad(any())).
find_value(Key, Config) ->
    ebt__option_m:to_error_m(ebt__xl_lists:kvfind(Key, Config),
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

-spec(version(config()) -> string()).
version(Config) -> definition(version, Config, "0.0.1").

-spec(appname_full(file:name(), config()) -> ebt__error_m:monad(string())).
appname_full(Dir, Config) ->
    ebt__do([ebt__error_m ||
        {_, Name, _} <- ebt_applib:load(Dir ++ "/src"),
        return(ebt__xl_string:join([Name, ebt_config:version(Config)], "-"))
    ]).

-spec(appname(file:name()) -> ebt__error_m:monad(atom())).
appname(Dir) ->
    ebt__do([ebt__error_m ||
        {_, Name, _} <- ebt_applib:load(Dir ++ "/src"),
        return(Name)
    ]).


-spec(files(atom(), config(), atom(), [string()], [string()]) -> [string()]).
files(Target, Config, Key, AdditionalMasks, DefaultMasks) ->
    files(ebt_config:value(Target, Config, []), Key, AdditionalMasks, DefaultMasks).

-spec(files([{atom(), term()}], [string()], [string()]) -> [string()]).
files(Config, AdditionalMasks, DefaultMasks) -> files(Config, files, AdditionalMasks, DefaultMasks).

-spec(files([{atom(), term()}] | atom(), atom(), [string()], [string()]) -> [string()]).
files(Target, Config, AdditionalMasks, DefaultMasks) when is_atom(Target) ->
    files(Target, Config, files, AdditionalMasks, DefaultMasks);
files(Config, Key, AdditionalMasks, DefaultMasks) when is_atom(Key) ->
    Files = ebt__xl_lists:kvfind(Key, Config, []),
    IncludeMasks = AdditionalMasks ++ ebt__xl_lists:kvfind(include, Files, DefaultMasks),
    ExcludeMasks = ebt__xl_lists:kvfind(exclude, Files, []),
    lists:subtract(ebt__xl_file:wildcards(IncludeMasks), ebt__xl_file:wildcards(ExcludeMasks)).

-spec(libraries(config()) -> [file:name()]).
libraries(Config) ->
    LibMasks = lists:map(fun(LibDir) -> LibDir ++ "/*" end, ebt_config:value(libraries, Config, [])),
    SortedLibs = lists:sort(fun compare/2, lists:map(fun libinfo/1, ebt__xl_file:wildcards(LibMasks))),
    FilteredLibs = lists:foldl(fun(L = {_Dir, Name, _Version}, Libraries) ->
        case lists:keymember(Name, 2, Libraries) of
            false -> [L | Libraries];
            true -> Libraries
        end
    end, [], SortedLibs),
    lists:map(fun({Dir, Name, Version}) -> Dir ++ "/" ++ Name ++ "-" ++ ebt__xl_string:join(Version, ".") end, FilteredLibs).

libinfo(Path) ->
    AbsPath = ebt__xl_file:absolute(Path),
    Dir = filename:dirname(AbsPath),
    LibName = filename:basename(AbsPath),
    NameTokens = string:tokens(LibName, "-"),
    {Name, [Version]} = lists:split(length(NameTokens) - 1, NameTokens),
    VersionList = string:tokens(Version, "."),
    {Dir, ebt__xl_string:join(Name, "-"), VersionList}.

compare({_, Name, Version1}, {_, Name, Version2}) -> Version1 > Version2;
compare({_, Name1, _Version1}, {_, Name2, _Version2}) -> Name1 > Name2.

definition(Key, Config, Default) ->
    case ebt__xl_lists:keyfind(Key, 2, Config) of
        {ok, {define, Key, Value}} -> Value;
        _ -> Default
    end.

definitions(Config) ->
    lists:map(fun({define, K, V}) ->
        {K, V}
    end, ebt__xl_lists:keyfilter(1, define, Config)).
