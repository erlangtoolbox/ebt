Erlang Build Tool
=================
Respects OTP Design Principles.

Version
-------
    {version, {shell, "echo -n 1.0.0"}}.

Compile
-------
    {compile, [
        {first, ["first_file.erl"]},
        {flags, [erlang,compile,flags]}
        {resources. ["resource_*.txt", "to_copy.*"]}
    ]}

Subdirs
-------
    {subdirs, ["dir1", "dir2"]}

Targets
-------
Do default target for phase 'prepare'. Default target for phase 'perform'
is 'otp-app'.
    {targets, [
        {prepare, [clean]},
        {perform, [escript, 'otp-app']}
    ]}

Dependencies
------------
Add target 'depends' to prepare phase
    {targets, [
        {prepare, [depends]},
    ]}.

Configure dependencies:
    {depends, [
        {dir, "./lib"},
        {repositories, [
            {"http://build.strikead.com:8081/repo", [
                {erlando, "2.0.0.vk-13"},
                {ktuo, "0.5.0.0-9"}
            ]}
        ]}
    ]}.

Libraries
---------
    {libraries, ["../lib"]}.

EScript
-------
    {targets, [
        {prepare, [clean]},
        {perform, [escript, 'otp-app']}
    ]}

    {escript, [
        {escript1, "-noshell -noinput"},
        {escript2, "-noshell -noinput"},
        {escript3, "-noshell -noinput"}
    ]}.

