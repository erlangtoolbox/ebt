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

-module(ebt_config).

-compile({parse_transform, do}).

-type(config() :: xl_lists:kvlist_at()).
-export_types([config/0, defaults/0]).

-export([read/3, value/3, value/4, find_value/2, find_value/3,
    outdir/1, outdir/3, version/1, appname_full/2, appname/1, app_outdir/3,
    outdir/2, ebin_outdir/2, files/3, files/4, libraries/1, libinfo/1, definition/3,
    definitions/1, update_buildinfo/3, buildinfo/1, output_dir/3, files/5]).

-spec(read(file:name(), config(), [{define, atom(), term()}]) -> error_m:monad(config())).
read(Filename, Defaults, Defines) ->
    case xl_file:exists(Filename) of
        {ok, true} ->
            do([error_m ||
                Config <- xl_file:read_terms(Filename),
                eval_definitions(lists:keymerge(1, lists:keysort(1, Config), lists:keysort(1, Defaults)), Defines)
            ]);
        {ok, false} -> {ok, Defaults ++ Defines};
        E -> E
    end.


eval_definitions(Config, Defines) ->
    WithDfines = lists:foldl(fun(ND, Cfg) ->
        Key = element(2, ND),
        case xl_lists:keyfind(Key, 2, Cfg) of
            {ok, E = {define, Key, _}} -> [ND | lists:delete(E, Cfg)];
            _ -> [ND | Cfg]
        end
    end, Config, Defines),
    xl_lists:emap(fun
        ({define, Key, Value}) ->
            do([error_m ||
                V <- evaluate(Value),
                return({define, Key, V})
            ]);
        (X) -> {ok, X}
    end, WithDfines).

evaluate({shell, Cmd}) -> xl_shell:command(Cmd);
evaluate(X) -> {ok, X}.

-spec(find_value(atom(), config()) -> error_m:monad(any())).
find_value(Key, Config) ->
    option_m:to_error_m(xl_lists:kvfind(Key, Config),
        xl_string:format("~p not found", [Key])).

-spec(find_value(atom(), config(), atom()) -> error_m:monad(any())).
find_value(Key, Config, InnerKey) ->
    Error = {error, xl_string:format("~p/~p not found", [Key, InnerKey])},
    case xl_lists:kvfind(Key, Config) of
        {ok, V} ->
            case xl_lists:kvfind(InnerKey, V) of
                undefined -> Error;
                X -> X
            end;
        undefined -> Error
    end.

-spec(value(atom(), config(), any()) -> any()).
value(Key, Config, Default) -> xl_lists:kvfind(Key, Config, Default).

-spec(value(atom(), config(), atom(), any()) -> any()).
value(Key, Config, InnerKey, Default) ->
    case xl_lists:kvfind(Key, Config) of
        {ok, V} -> xl_lists:kvfind(InnerKey, V, Default);
        undefined -> Default
    end.

-spec(outdir(config()) -> error_m:monad(string())).
outdir(Config) ->
    do([error_m ||
        Dir <- ebt_config:find_value(outdir, Config),
        xl_file:mkdirs(Dir),
        return(Dir)
    ]).

-spec(outdir(atom(), config()) -> error_m:monad(string())).
outdir(Kind, Config) ->
    do([error_m ||
        Parent <- outdir(Config),
        Dir <- return(filename:absname(xl_string:join([Parent, Kind], "/"))),
        xl_file:mkdirs(Dir),
        return(Dir)
    ]).

-spec(outdir(atom(), config(), string()) -> error_m:monad(string())).
outdir(Kind, Config, Suffix) ->
    do([error_m ||
        Parent <- outdir(Kind, Config),
        Dir <- return(xl_string:join([Parent, Suffix], "/")),
        xl_file:mkdirs(Dir),
        return(Dir)
    ]).

-spec(app_outdir(atom(), file:name(), config()) -> error_m:monad(string())).
app_outdir(Kind, Dir, Config) ->
    do([error_m ||
        App <- appname_full(Dir, Config),
        outdir(Kind, Config, App)
    ]).

-spec(output_dir({atom(), file:name()} | atom() | file:name(), file:name(), config()) ->
    error_m:monad(file:name())).
output_dir({config, Target, Key}, Dir, Config) ->
    do([error_m ||
        Value <- find_value(Target, Config, Key),
        output_dir(Value, Dir, Config)
    ]);
output_dir({outdir, Kind, Suffix}, Dir, Config) ->
    do([error_m ||
        Parent <- output_dir({outdir, Kind}, Dir, Config),
        TargetDir <- return(xl_string:join([Parent, Suffix], "/")),
        xl_file:mkdirs(TargetDir),
        return(TargetDir)
    ]);
output_dir({outdir, Kind}, Dir, Config) when is_atom(Kind) ->
    do([error_m ||
        App <- appname_full(Dir, Config),
        OutDir <- outdir(Config),
        TargetDir <- return(xl_string:join([OutDir, Kind, App], "/")),
        xl_file:mkdirs(TargetDir),
        return(TargetDir)
    ]);
output_dir({outdir, Path}, _Dir, Config) ->
    do([error_m ||
        OutDir <- outdir(Config),
        TargetDir <- return(xl_string:join([OutDir, Path], "/")),
        xl_file:mkdirs(TargetDir),
        return(TargetDir)
    ]).



-spec(ebin_outdir(file:name(), config()) -> error_m:monad(string())).
ebin_outdir(Dir, Config) ->
    do([error_m ||
        AppProdDir <- app_outdir(production, Dir, Config),
        EbinDir <- return(xl_string:join([AppProdDir, "ebin"], "/")),
        xl_file:mkdirs(EbinDir),
        return(EbinDir)
    ]).

-spec(version(config()) -> string()).
version(Config) -> definition(version, Config, "0.0.1").

-spec(appname_full(file:name(), config()) -> error_m:monad(string())).
appname_full(Dir, Config) ->
    do([error_m ||
        {_, Name, _} <- et_code_appdesc:load(Dir ++ "/src"),
        return(xl_string:join([Name, ebt_config:version(Config)], "-"))
    ]).

-spec(appname(file:name()) -> error_m:monad(atom())).
appname(Dir) ->
    do([error_m ||
        {_, Name, _} <- et_code_appdesc:load(Dir ++ "/src"),
        return(Name)
    ]).

-spec(update_buildinfo(config(), atom(), term()) -> config()).
update_buildinfo(Config, Key, Value) ->
    Info = value(buildinfo, Config, []),
    lists:keystore(buildinfo, 1, Config, {buildinfo, lists:keystore(Key, 1, Info, {Key, Value})}).

-spec(buildinfo(config()) -> [{atom(), term()}]).
buildinfo(Config) -> value(buildinfo, Config, []).

-spec(files(atom(), config(), atom(), [string()], [string()]) -> [string()]).
files(Target, Config, Key, AdditionalMasks, DefaultMasks) ->
    files(ebt_config:value(Target, Config, []), Key, AdditionalMasks, DefaultMasks).

-spec(files([{atom(), term()}], [string()], [string()]) -> [string()]).
files(Config, AdditionalMasks, DefaultMasks) -> files(Config, files, AdditionalMasks, DefaultMasks).

-spec(files([{atom(), term()}] | atom(), atom(), [string()], [string()]) -> [string()]).
files(Target, Config, AdditionalMasks, DefaultMasks) when is_atom(Target) ->
    files(Target, Config, files, AdditionalMasks, DefaultMasks);
files(Config, Key, AdditionalMasks, DefaultMasks) when is_atom(Key) ->
    Files = xl_lists:kvfind(Key, Config, []),
    IncludeMasks = AdditionalMasks ++ xl_lists:kvfind(include, Files, DefaultMasks),
    ExcludeMasks = xl_lists:kvfind(exclude, Files, []),
    lists:subtract(
        xl_file:wildcards(filterout(IncludeMasks, Config)),
        xl_file:wildcards(filterout(ExcludeMasks, Config))
    ).

filterout(Masks, Config) ->
    lists:map(fun
        (Mask) when is_tuple(Mask) -> output_dir(Mask, undefined, Config);
        (Mask) -> Mask
    end, Masks).


-spec(libraries(config()) -> [file:name()]).
libraries(Config) ->
    LibMasks = lists:map(fun(LibDir) -> LibDir ++ "/*" end, ebt_config:value(libraries, Config, [])),
    SortedLibs = lists:sort(fun compare/2, lists:map(fun libinfo/1, xl_file:wildcards(LibMasks))),
    FilteredLibs = lists:foldl(fun(L = {_Dir, Name, _Version}, Libraries) ->
        case lists:keymember(Name, 2, Libraries) of
            false -> [L | Libraries];
            true -> Libraries
        end
    end, [], SortedLibs),
    lists:map(fun({Dir, Name, Version}) ->
        Dir ++ "/" ++ Name ++ "-" ++ xl_string:join(Version, ".") end, FilteredLibs).

libinfo(Path) ->
    AbsPath = xl_file:absolute(Path),
    Dir = filename:dirname(AbsPath),
    LibName = filename:basename(AbsPath),
    NameTokens = string:tokens(LibName, "-"),
    {Name, [Version]} = lists:split(length(NameTokens) - 1, NameTokens),
    VersionList = string:tokens(Version, "."),
    {Dir, xl_string:join(Name, "-"), VersionList}.

compare({_, Name, Version1}, {_, Name, Version2}) -> Version1 > Version2;
compare({_, Name1, _Version1}, {_, Name2, _Version2}) -> Name1 > Name2.

definition(Key, Config, Default) ->
    case xl_lists:keyfind(Key, 2, Config) of
        {ok, {define, Key, Value}} -> Value;
        _ -> Default
    end.

definitions(Config) ->
    lists:map(fun({define, K, V}) ->
        {K, V}
    end, xl_lists:keyfilter(1, define, Config)).
