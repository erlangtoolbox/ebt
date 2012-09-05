#!/usr/bin/env escript

-include_lib("kernel/include/file.hrl").

-define(SOURCES, "ebt/src").
-define(BUILD_DIR, "bootstrap/production/ebin").
-define(LIB_DIRS, "lib/*/ebin").
-define(TARGET, "bootstrap/dist/ebt").

main(_Args) ->
    ok = filelib:ensure_dir(?BUILD_DIR ++ "/x"),
    true = code:add_path(?BUILD_DIR),
    [code:add_path(Dir) || Dir <- filelib:wildcard(?LIB_DIRS)],
    case make:files(filelib:wildcard(?SOURCES ++ "/*.erl"),
        [{outdir, ?BUILD_DIR}, debug_info, warnings_as_errors]) of
            up_to_date -> ok;
            error ->
                io:format(standard_error, "Failed to compile files!\n", []),
                halt(1)
    end,
    ok = filelib:ensure_dir(?TARGET),
    case zip:create("memory",
        read_files([?BUILD_DIR ++ "/*", ?SOURCES ++ "/*.app", ?LIB_DIRS ++ "/*"]) ++
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

