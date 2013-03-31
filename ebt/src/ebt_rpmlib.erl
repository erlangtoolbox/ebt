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
