#!/usr/bin/env escript

-include_lib("kernel/include/file.hrl").

-define(SOURCES, "ebt/src").
-define(BUILD_DIR, "out/bootstrap/production/ebin").
-define(LIB_DIRS, "lib/*/ebin").
-define(TARGET, "out/bootstrap/dist/ebt").

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
    case zip:create("memory", load_files([
    	?BUILD_DIR ++ "/*",
    	?SOURCES ++ "/*.app",
    	?LIB_DIRS ++ "/*"
    ]), [memory]) of
        {ok, {"memory", ZipBin}} ->
            Header =
            	"#!/usr/bin/env escript\n"
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

load_files(Wildcards) ->
	[read_file(Filename) ||
		Wildcard <- Wildcards,
		Filename <- filelib:wildcard(Wildcard)].

read_file(Filename) ->
    {lists:last(filename:split(Filename)), file_contents(Filename)}.

file_contents(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    Bin.
