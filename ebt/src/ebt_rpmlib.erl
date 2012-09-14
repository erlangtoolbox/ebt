%% Copyright
-module(ebt_rpmlib).
-author("volodymyr.kyrychenko@strikead.com").

-compile({parse_transform, do}).

%% API
-export([prepare_environment/1, spec_header/5]).


prepare_environment(RPMBuildDir) ->
    do([error_m ||
        ebt_xl_file:mkdirs(RPMBuildDir ++ "/BUILD"),
        ebt_xl_file:mkdirs(RPMBuildDir ++ "/BUILDROOT"),
        ebt_xl_file:mkdirs(RPMBuildDir ++ "/RPMS"),
        ebt_xl_file:mkdirs(RPMBuildDir ++ "/SOURCES"),
        ebt_xl_file:mkdirs(RPMBuildDir ++ "/SPECS"),
        ebt_xl_file:mkdirs(RPMBuildDir ++ "/SRPMS")
    ]).


spec_header(AppName, Version, BuildNumber, Headers, RPMSDir) ->
    do([error_m ||
        Values <- return([
            {'Name', AppName},
            {'Release', BuildNumber ++ "%{?dist}"},
            {'Version', Version} | resolve_requires(Headers, RPMSDir)
        ]),
        return(ebt_xl_string:join([ebt_xl_string:format("~s: ~s~n", [N, V]) || {N, V} <- Values]))
    ]).


resolve_requires(Headers, RPMSDir) when is_list(Headers) ->
    [resolve_requires(H, RPMSDir) || H <- Headers];
resolve_requires(H = {'Requires', Package}, RPMSDir) ->
    case try_detect(ebt_xl_string:format("rpm -q ~s --qf '%{version}-%{release}'", [Package]), Package) of
        {ok, Header} -> Header;
        undefined ->
            case lists:reverse(lists:sort(filelib:wildcard(ebt_xl_string:join([RPMSDir, "/*/", Package, "*"])))) of
                [File | _] ->
                    case try_detect(ebt_xl_string:format("rpm -q -p '~s' --qf '%{version}-%{release}'", [File]), Package) of
                        {ok, Header} -> Header;
                        undefined -> H
                    end;
                _ -> H
            end
    end;
resolve_requires(X, _RRPMDir) -> X.

try_detect(Command, Package) ->
    io:format("detecting version: ~s~n", [Command]),
    case ebt_xl_shell:command(Command) of
        {ok, Version} ->
            io:format("detected ~s-~s~n", [Package, Version]),
            {ok, {'Requires', ebt_xl_string:join([Package, "=", Version], " ")}};
        {error, Stdout} ->
            io:format("failed detection ~s: ~s", [Package, Stdout]),
            undefined
    end.
