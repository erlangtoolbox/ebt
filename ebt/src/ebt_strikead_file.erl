-module(ebt_strikead_file).

-include_lib("kernel/include/file.hrl").

-compile({parse_transform, do}).

-behaviour(ebt_strikead_autoresource).
-export([auto_open/1, auto_close/1, using/3]).
-export([list_dir/2, compile_mask/1, find/2, exists/1, mkdirs/1, write_terms/2,
    read_terms/1, read_files/1, read_files/2, copy_if_exists/2, copy_filtered/3,
    absolute/1]).
-export([read_file/1, delete/1, make_symlink/2, write_file/2, ensure_dir/1,
    list_dir/1, copy/2, open/2, close/1, change_mode/2, read_file_info/1]).


list_dir(Dir, Filter) when is_function(Filter) ->
    case list_dir(Dir) of
        {ok, Listing} -> {ok, lists:filter(Filter, Listing)};
        E -> E
    end;

list_dir(Dir, Mask) when is_list(Mask) -> list_dir(Dir, compile_mask(Mask)).

list_dir(Dir) -> ebt_strikead_io:apply_io(file, list_dir, [Dir]).

find(Dir, Mask) when is_list(Mask) ->
    case list_dir(Dir, Mask) of
        {ok, [F | _]} -> {ok, F};
        {ok, []} -> not_found;
        E -> E
    end.

compile_mask(Mask) ->
    fun(X) ->
        case re:run(X, compile_mask(Mask, [])) of
            {match, _} -> true;
            _ -> false
        end
    end.
compile_mask([], Acc) -> Acc;
compile_mask([$. | T], Acc) -> compile_mask(T, Acc ++ "\.");
compile_mask([$* | T], Acc) -> compile_mask(T, Acc ++ ".*");
compile_mask([$? | T], Acc) -> compile_mask(T, Acc ++ ".");
compile_mask([H | T], Acc) -> compile_mask(T, Acc ++ [H]).

-spec exists/1 :: (file:name()) -> error_m:monad(boolean()).
exists(Path) ->
    case ebt_strikead_io:apply_io(file, read_file_info, [Path]) of
        {ok, _} -> {ok, true};
        {error, {enoent, _, _}} -> {ok, false};
        E -> E
    end.

ensure_dir(Path) -> ebt_strikead_io:apply_io(filelib, ensure_dir, [Path]).

mkdirs(Path) -> ensure_dir(filename:join(Path, "X")).

-spec read_terms/1 :: (file:name()) -> {ok, [term()]} | ebt_strikead_io:posix_error().
read_terms(Filename) -> ebt_strikead_io:apply_io(file, consult, [Filename]).

write_terms(File, L) when is_list(L) ->
    R = do([error_m ||
        ensure_dir(File),
        using(File, [write], fun(F) ->
            lists:foreach(fun(X) -> io:format(F, "~p.~n", [X]) end, L)
        end)
    ]),
    case R of
        {ok, ok} -> ok;
        X -> X
    end;

write_terms(File, L) -> write_terms(File, [L]).

-spec copy(file:filename(), file:filename()) -> error_m:monad(ok).
copy(Src, Dst) ->
    case type(Src) of
        {ok, regular} ->
            DestinationFile = filename:join(Dst, filename:basename(Src)),
            do([error_m ||
                ensure_dir(DestinationFile),
                ebt_strikead_io:apply_io(file, copy, [Src, DestinationFile]),
                ok
            ]);
        {ok, directory} ->
            do([error_m ||
                Files <- list_dir(Src),
                NewDst <- return(filename:join(Dst, filename:basename(Src))),
                mkdirs(NewDst),
                ebt_strikead_lists:eforeach(fun(F) ->
                    copy(filename:join(Src, F), NewDst)
                end, Files)
            ]);
        {ok, T} -> {error, {cannot_copy, T, [Src, Dst]}};
        E -> E
    end.

type(Path) ->
    case read_file_info(Path) of
        {ok, #file_info{type = T}} -> {ok, T};
        E -> E
    end.

-spec copy_filtered(file:name(), [string()], file:name()) -> error_m:monad(ok).
copy_filtered(SrcDir, Wildcards, DstDir) ->
    ebt_strikead_lists:eforeach(fun(F) ->
        ebt_strikead_file:copy(F, DstDir)
    end, [F || WC <- Wildcards, F <- filelib:wildcard(SrcDir ++ "/" ++ WC)]).

-spec copy_if_exists/2 :: (file:name(), file:name()) -> error_m:monad(ok).
copy_if_exists(Src, Dst) ->
    case ebt_strikead_file:exists(Src) of
        {ok, true} -> copy(Src, Dst);
        {ok, _} -> ok;
        E -> E
    end.

read_file(Path) -> ebt_strikead_io:apply_io(file, read_file, [Path]).
write_file(Path, Data) ->
    case ensure_dir(Path) of
        ok -> ebt_strikead_io:apply_io(file, write_file, [Path, Data]);
        E -> E
    end.
open(File, Mode) -> ebt_strikead_io:apply_io(file, open, [File, Mode]).
close(Fd) -> ebt_strikead_io:apply_io(file, close, [Fd]).
make_symlink(Target, Link) -> ebt_strikead_io:apply_io(file, make_symlink, [Target, Link]).
read_file_info(Path) -> ebt_strikead_io:apply_io(file, read_file_info, [Path]).
change_mode(Path, Mode) -> ebt_strikead_io:apply_io(file, change_mode, [Path, Mode]).

absolute(Path) ->
    Abs = lists:reverse(lists:filter(fun(X) -> X /= "." end,
        filename:split(filename:join([filename:absname(Path)])))),
    filename:join(absolute(Abs, [], 0)).

absolute([], Acc, _) -> Acc;
absolute([".." | T], Acc, Skip) -> absolute(T, Acc, Skip + 1);
absolute([H | T], Acc, 0) -> absolute(T, [H | Acc], 0);
absolute(["/"], Acc, _) -> ["/" | Acc];
absolute([_ | T], Acc, Skip) -> absolute(T, Acc, Skip - 1).

-spec read_files/1 :: ([string()]) -> error_m:monad([{string(), binary()}]).
read_files(Wildcards) -> read_files(Wildcards, name).

-spec read_files/2 :: ([string()], name | {base, file:name()}) ->
    error_m:monad([{string(), binary()}]).
read_files(Wildcards, Option) ->
    ebt_strikead_lists:eflatten(ebt_strikead_lists:emap(fun(Name) ->
        case type(Name) of
            {ok, directory} -> read_files([Name ++ "/*"], Option);
            {ok, regular} ->
                case read_file(Name) of
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
                        {ok, {N, Bin}};
                    E -> E
                end;
            {ok, T} -> {error, {cannot_read, T, Name}};
            E -> E
        end
    end, [Filename || Wildcard <- Wildcards, Filename <- filelib:wildcard(Wildcard)])).

%todo handle symlinks
delete(Path) ->
    case type(Path) of
        {ok, regular} ->
            ebt_strikead_io:apply_io(file, delete, [Path]);
        {ok, directory} ->
            do([error_m ||
                Files <- list_dir(Path),
                ebt_strikead_lists:eforeach(fun(P) -> delete(filename:join(Path, P)) end, Files),
                ebt_strikead_io:apply_io(file, del_dir, [Path])
            ]);
        {ok, T} -> {error, {cannot_delete, T, [Path]}};
        {error, {enoent, _, _}} -> ok;
        E -> E
    end.


%%
% autoresource
%%
auto_open([File, Mode]) -> open(File, Mode).
auto_close(D) -> close(D).
using(File, Mode, F) -> ebt_strikead_auto:using(?MODULE, [File, Mode], F).
