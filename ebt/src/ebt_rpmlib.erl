%% Copyright
-module(ebt_rpmlib).
-author("volodymyr.kyrychenko@strikead.com").

-compile({parse_transform, ebt__do}).

%% API
-export([prepare_environment/1, spec_header/5]).


prepare_environment(RPMBuildDir) ->
    ebt__do([ebt__error_m ||
        ebt__xl_file:mkdirs(RPMBuildDir ++ "/BUILD"),
        ebt__xl_file:mkdirs(RPMBuildDir ++ "/BUILDROOT"),
        ebt__xl_file:mkdirs(RPMBuildDir ++ "/RPMS"),
        ebt__xl_file:mkdirs(RPMBuildDir ++ "/SOURCES"),
        ebt__xl_file:mkdirs(RPMBuildDir ++ "/SPECS"),
        ebt__xl_file:mkdirs(RPMBuildDir ++ "/SRPMS")
    ]).


spec_header(Name, Version, BuildNumber, Headers, RPMSDir) ->
    ebt__do([ebt__error_m ||
        Values <- return([
            {'Name', Name},
            {'Release', BuildNumber ++ "%{?dist}"},
            {'Version', Version} | resolve_requires(Headers, RPMSDir)
        ]),
        return(ebt__xl_string:join([ebt__xl_string:format("~s: ~s~n", [N, V]) || {N, V} <- Values]))
    ]).


resolve_requires(Headers, RPMSDir) when is_list(Headers) ->
    [resolve_requires(H, RPMSDir) || H <- Headers];
resolve_requires(H = {'Requires', Package}, RPMSDir) ->
    case try_detect(ebt__xl_string:format("rpm -q ~s --qf '%{version}-%{release}'", [Package]), Package) of
        {ok, Header} -> Header;
        undefined ->
            case lists:reverse(lists:sort(filelib:wildcard(ebt__xl_string:join([RPMSDir, "/*/", Package, "*"])))) of
                [File | _] ->
                    case try_detect(ebt__xl_string:format("rpm -q -p '~s' --qf '%{version}-%{release}'", [File]), Package) of
                        {ok, Header} -> Header;
                        undefined -> H
                    end;
                _ -> H
            end
    end;
resolve_requires(X, _RRPMDir) -> X.

try_detect(Command, Package) ->
    io:format("detecting version: ~s~n", [Command]),
    case ebt__xl_shell:command(Command) of
        {ok, Version} ->
            io:format("detected ~s-~s~n", [Package, Version]),
            {ok, {'Requires', ebt__xl_string:join([Package, "=", Version], " ")}};
        {error, Stdout} ->
            io:format("failed detection ~s: ~s", [Package, Stdout]),
            undefined
    end.
