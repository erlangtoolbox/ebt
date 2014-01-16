# Erlang Build Tool

The purpose of creating EBT was trying incapsulate building by convention
approach to minimalize efforts of doing same things differently. Following
naming convention with EBT you can have erlagn application build really easy
configuring the thing only required to be configured. EBT is not just build tool
- it does dependency management for Erlang. EBT can build erlang project from sources
to erlang release.

It follows OTP Design Principles: http://www.erlang.org/doc/design_principles/


NOTICE: Erlang version requireid is Erlang R15.

Before start clone latests EBT sources from http://github.com/strikead/ebt and type execute make.sh.
After successful build executable will be located into out/production/ebt-X.Y.Z/bin/ebt.
Put it somewhethe in the search path and assuming you have erlang R15 installed in your
system you're good to go.

## Hello world project

### Hello EBT
Let's try Hello EBT project by example. Creating src directory with Erlang module 'src/hello.erl'.

    -module(hello).
    -export([hello/0]).

    hello() -> io:format("Hello EBT").

Put this file into src/hello.erl. Then make it erlang application. Create src/hello.app:

    {application, hello, [
        {description, "Hello EBT"},
        {registered, []},
        {applications, [
            kernel,
            stdlib
        ]}
    ]}.

Type ebt in the directory and you will see successful build of the our Hello project.
Build result can be foud at out directory. out./production contains compiled version of
our OTP aaplication and out/dist contains erlang EZ package hello-0.0.1.ez. You can look
at the application description file out/production/hello-0.0.1/ebin/hello.app. As you
can see there is version and modules defined automatically. No manual intervention
needed! This is it - compilation is done.

### Hello EBT EUnit
Now try adding some tests. Lets create test/hello_tests.erl with eunit test.

    -module(hello_tests).

    -include_lib("eunit/include/eunit.hrl").

    hello_test() ->
        ?assertEqual(ok, hello:hello()).

Then type ebt again. You will see in output that eunit found your tests and perfoprmed it:

    eunit:
            [eunit] test hello
            [eunit]   Test passed.

### Version

Having application version 0.0.1 is fun but let's change it. Now is the time to create actual
ebt configuration file that will contain everything we need to configure. Default filename is
ebt.config. It is erlang term file (see file:consult). Now let's define our application version.
The contents of the ebt.config:

    {define, version, "1.0.0"}.

Type ebt. You'll see that your erlang application is now build with version 1.0.0.
Having it statically defined is good but not enough. The sourfce of the defition could
be shell command invocation. Let's do that:

    {define, version, {shell, "echo -n 1.0.1"}}.

Using this mechanism you can provide desired version from outer sources for EBT build.

### Modules

Let's make our application modular. For the start create module hello in the root directory and
move src and test directoryes into it. After this project structure should look like this:

    hello
        src
            hello.erl
            hello.app
        test
            hello_tests.erl
    ebt.config

Now we should tell our ebt.cofig that we are having multimodular structure. For this we should
create build profile (see profiles) and cofigure it. Nopw our ebt.config file looks like this:

    {define, version, {shell, "echo -n 1.0.1"}}.

    {profiles, [
        {default, [
            {subdirs, ["hello"]}
        ]}
    ]}.

Trying to build we will get an error:

    BUILD FAILED: failed to locate single app file in ./src

This arror is caused because by default ebt tryes to build OTP application and every OTP application
should at least have and application descriptor in it's src dir. Current directory is not OTP
application and we should tell ebt to get over it and do some work in subdirs. To achieve this we
should tell it not to perform any kind of work in current directory. ebt.config:

    {define, version, {shell, "echo -n 1.0.1"}}.

    {profiles, [
        {default, [
            {subdirs, ["hello"]},
            {perform, []}
        ]}
    ]}.

Configuring perform with empty list tells ebt to perform empty list of tasks. Type ebt and you'll see that
everything was build an everything on their places. To see complex multimodule project look at the example
directory in the ebt github repository.



## Profiles

Public Repository http://code.google.com/p/erlang-build-tool/downloads/list

Tuitorial
---------


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
is 'package'.
    {profiles, [
        {default, [
            {perform, [package]}
        ]}
    ]}.

Dependencies
------------
Add target 'depends' to prepare phase
    {profiles, [
        {default, [
            {prepare, [depends]},
            {perform, [compile]}
        ]}
    ]}.

Configure dependencies:
    {depends, [
        {dir, "./lib"},
        {repositories, [
            {"http://erlang-build-tool.googlecode.com/files", [
                {erlandox, "1.0.4"}
            ]}
        ]}
    ]}.

Libraries
---------
    {libraries, ["../lib"]}.

EScript
-------
    {profiles, [
        {default, [
            {perform, [escript]}
        ]}
    ]}.

    {escript, [
        {escript1, "-noshell -noinput", ["priv/*"]},
        {escript2, "-noshell -noinput", ["priv/*"]},
        {escript3, "-noshell -noinput", ["priv/*"]}
    ]}.

