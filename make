#!/usr/bin/env escript

-include_lib("kernel/include/file.hrl").

-define(SOURCES_FIRST, ["ebt/src/ebt__do.erl", "ebt/src/ebt__cut.erl", "ebt/src/ebt__monad.erl", "ebt/src/ebt__monad_plus.erl", "ebt/src/ebt__xl_autoresource.erl"]).
-define(SOURCES, "ebt/src").
-define(INCLUDES, "ebt/include").
-define(BUILD_DIR, "bootstrap/production/ebin").
-define(TARGET, "bootstrap/dist/ebt").
-define(DEPS, [
    {"https://github.com/jcomellas/getopt/archive/master.zip", "getopt.zip", "getopt-master",[
        {".", ["getopt.erl"], []}
    ],["getopt"]
    },
    {"https://github.com/vladimirk/erlandox/archive/master.zip", "erlandox.zip", "erlandox-master",[
        {".", ["do.erl", "cut.erl", "monad.erl", "monad_plus.erl", "option_m.erl", "error_m.erl"], ["monad_specs.hrl"]}
    ], ["do", "monad", "cut", "option_m", "error_m"]
    },
    {"https://github.com/strikead/erlangxl/archive/develop.zip", "erlangxl.zip", "erlangxl-develop",[
        {"xl_stdlib", ["xl_auto.erl", "xl_autoresource.erl", "xl_convert.erl", "xl_lists.erl", "xl_stream.erl", "xl_string.erl"], []},
        {"xl_io", ["xl_escript.erl", "xl_file.erl", "xl_io.erl", "xl_shell.erl", "xl_zip.erl"], []}
    ], ["xl_auto", "xl_convert", "xl_escript", "xl_file", "xl_io", "xl_lists", "xl_shell",
        "xl_stream", "xl_string", "xl_zip", "do", "error_m", "option_m"]
    }
]).

main(_Args) ->
    deps(),
    ok = filelib:ensure_dir(?BUILD_DIR ++ "/x"),
    true = code:add_path(?BUILD_DIR),
    compile(?SOURCES_FIRST),
    compile(filelib:wildcard(?SOURCES ++ "/*.erl")),
    ok = filelib:ensure_dir(?TARGET),
    case zip:create("memory",
        read_files([?BUILD_DIR ++ "/*", ?SOURCES ++ "/*.app"]) ++
        read_files(["ebt/priv"], {base, "ebt"}), [memory]) of
        {ok, {"memory", ZipBin}} ->
            Header =
                "#!/usr/bin/env escript\n"
                "%%\n"
                "%%! -noshell -noinput\n",
            case file:write_file(?TARGET, iolist_to_binary([Header, ZipBin])) of
                ok -> ok;
                {error, Error} ->
                    io:format(standard_error, "Failed to write ~p script: ~p\n", [?TARGET, Error]),
                    halt(1)
            end,
            {ok, #file_info{mode = Mode}} = file:read_file_info(?TARGET),
            ok = file:change_mode(?TARGET, Mode bor 8#00100);
        {error, Error} ->
            io:format(standard_error, "Failed to construct ~p escript: ~p\n", [?TARGET, Error]),
            halt(1)
    end,
    io:format("~s is bootstrapped!~n", [?TARGET]),
    io:format(os:cmd(?TARGET)).

compile(Sources) ->
    case make:files(Sources, [{outdir, ?BUILD_DIR}, debug_info, {i, ?INCLUDES}]) of
            up_to_date -> ok;
            error ->
                io:format(standard_error, "Failed to compile files!\n", []),
                halt(1)
    end.


type(Path) ->
    case file:read_file_info(Path) of
        {ok, #file_info{type = T}} -> {ok, T};
        E -> E
    end.

read_files(Wildcards) -> read_files(Wildcards, name).

read_files(Wildcards, Option) ->
    lists:flatten(lists:map(fun(Name) ->
         case type(Name) of
             {ok, directory} -> read_files([Name ++ "/*"], Option);
             {ok, regular} ->
                 case file:read_file(Name) of
                     {ok, Bin} ->
                         N = case Option of
                                 name ->
                                 lists:last(filename:split(Name));
                             {base, BaseDir} ->
                                 AbsBase = absolute(BaseDir),
                                 AbsName = absolute(Name),
                                 string:substr(AbsName, string:len(AbsBase) + 2);
                             _ -> {error, {badarg, Option}}
                         end,
                         {N, Bin};
                     E -> E
                 end;
             {ok, T} -> {error, {cannot_read, T, Name}};
             E -> E
         end
    end, [Filename || Wildcard <- Wildcards, Filename <- filelib:wildcard(Wildcard)])).

absolute(Path) ->
    Abs = lists:reverse(lists:filter(fun(X) -> X /= "." end,
        filename:split(filename:join([filename:absname(Path)])))),
    filename:join(absolute(Abs, [], 0)).

absolute([], Acc, _) -> Acc;
absolute([".." | T], Acc, Skip) -> absolute(T, Acc, Skip + 1);
absolute([H | T], Acc, 0) -> absolute(T, [H | Acc], 0);
absolute(["/"], Acc, _) -> ["/" | Acc];
absolute([_ | T], Acc, Skip) -> absolute(T, Acc, Skip - 1).


deps() ->
    inets:start(),
    ssl:start(),
    lists:foreach(fun({Url, Name, SourceDir, Modules, Replacements}) ->
        LocalFile = ".deps/" ++ Name,
        io:format("downloading ~s to ~s~n", [Url, LocalFile]),
        ok = filelib:ensure_dir(LocalFile),
        case file:read_file_info(LocalFile) of
            {ok, _} -> ok;
            {error, _} ->
                {ok, _} = httpc:request(get, {Url, []}, [], [{stream, LocalFile}])
        end,
        zip:unzip(LocalFile, [{cwd, ".deps"}, verbose]),
        lists:foreach(fun({Module, Sources, Includes}) ->
            fix(Sources, Replacements, SourceDir ++ "/" ++ Module ++ "/src", "ebt/src"),
            fix(Includes, Replacements, SourceDir ++ "/" ++ Module ++ "/include", "ebt/include")
        end, Modules)
    end, ?DEPS).

fix(Files, Replacements, SourceDir, DestDir) ->
        lists:foreach(fun(File) ->
            {ok, Content} = file:read_file(io_lib:format(".deps/~s/~s",[SourceDir, File])),
            FixedContents = lists:foldl(fun(Rep, Cnt) ->
                binary:replace(Cnt, list_to_binary(Rep), <<"ebt__">>, [{insert_replaced, 5}, global])
            end, Content, Replacements),
            DestFile = io_lib:format("~s/ebt__~s", [DestDir, File]),
            ok = filelib:ensure_dir(DestFile),
            ok = file:write_file(DestFile, FixedContents)
        end, Files).
