# Erlang Build Tool

The purpose of creating EBT was trying incapsulate building by convention
approach to minimalize efforts of doing same things differently. Following
naming convention with EBT you can have erlagn application build really easy
configuring the thing only required to be configured. EBT is not just build tool -
it does dependency management for Erlang. EBT can build erlang project from sources
to erlang release.

It follows OTP Design Principles: http://www.erlang.org/doc/design_principles/


NOTICE: Erlang version requireid is Erlang R15.

Before start clone latests EBT sources from http://github.com/erlangtoolbox/ebt and type execute make.sh.
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
Having it statically defined is good but not enough. The source of the defition could
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

This error is caused because by default ebt tries to build OTP application and every OTP application
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
everything was build an everything on their places. For more complex complex multimodular project look at
the example directory in the ebt github repository.

# Key Concepts

## Targets

Single worker of the ebt build process is target. Targets depend on each other. For instance before eunit
tests could be performed project should be compiled. Default dependecy configuration is defined in the ebt.app
file. It looks like this:

     {tasks, [
            {modules, [
                {clean, ebt_task_clean},
                {compile, ebt_task_compile},
                {eunit, ebt_task_eunit},
                {package, ebt_task_package},
                .....
            ]},
            {targets, [
                {compile, [template, leex, yecc, protoc, cc]},
                {eunit, [compile, cover]},
                ....
                {package, [cover_analyse, edoc, git_info]},
                ....
            ]}

First configuration is modules. It assigns symbolical target 'compile' erlang module
implementing this feature 'ebt_task_compile'. Targets defines dependencies.
For instance before compile there should be some other targets performed.

## Profiles

EBT configuration could have multiple profiles for build. For instance generating application
release is heavy time consuming task and for the sake of time we can configure different profiles
for the whole distribution and for unit tests only. Default profile name is 'default'. Profile
configuration has 3 settings:
* subdirs - list of subdirectories to attend
* prepare - lists of targets to execute before going to subdirs
* perform - lists of targets to execute after exiting from subdirs.

By default prepare does nothing and perform builds ez package.

Profile could be invoked with command line parameter:

    ebt -p profile_name

In the case of multimodule project every subdir will be invoked with the same profile.
If this profile is not defined default profile will be used.

## Dependency Management

Dependency management is on of the key features of EBT. There is public repo with couple
of libraries already set at google code  http://code.google.com/p/erlang-build-tool/downloads/list
It's good for browsing and any help with public server will be appreciated. Also build server for public
libraries is on the way so build configuration could be submitted to it and build result will be
published into the public repository.

Dependency configuration example:

    {depends, [
        {dir, "./lib"},
        {repositories, [
            {"http://erlang-build-tool.googlecode.com/files", [
                {erlandox, "1.0.5"},
                {xl_stdlib, "1.2.11.358"},
                {getopt, "0.7.1"}
            ]}
        ]}
    ]}.

EBT will download ez packages and will use it in the build. Dependency download is target itself
so for things to be done it should be configured in the prepare phase of the root ebt.config.

## Libraries

Library configuration:

    {libraries, ["./lib", "./out/production"]}.

Library paths should be configured if there are dependencies to the libs downloaded or modules
just built by other modules.


## Extending EBT


### Task implementation

ebtx/src/ebtx.app:

    {application, ebtx, [
        {description, "Hello EBT Extentions"},
        {registered, []},
        {applications, [
            kernel,
            stdlib
        ]}
    ]}.

ebtx/src/ebtx.erl:

    -module(ebtx).

    -behaviour(ebt_task).

    -export([perform/3]).

    perform(_Target, _Dir, _Config) ->
        io:format("Hello from EBT!~n").

ebtx/ebt.config:

    {libraries, ["./lib"]}.

    {profiles, [
        {default, [
            {prepare, [depends]}
        ]}
    ]}.

    {depends, [
        {dir, "./lib"},
        {repositories, [
            {"http://erlang-build-tool.googlecode.com/files", [
                {ebt, "1.4.3"}
            ]}
        ]}
    ]}.

### Configure custom target in ebt.config

hello/ebt.config:

    {libraries, ["../out/production"]}.

    {tasks, [
        {modules, [
            {ebtx, ebtx}
        ]},
        {targets, [
            {compile, [ebtx]}
        ]}
    ]}.

This means ebtx target will be executed before compile.

### Add subdir to root ebt.config

    {define, version, {shell, "echo -n 1.0.1"}}.

    {profiles, [
        {default, [
            {subdirs, ["ebtx", "hello"]},
            {perform, []}
        ]}
    ]}.

### Hooray!

Type ebt and see hello project built with custom target invoked:

    ...........
    perform => cc at .
    cc:
    perform => ebtx at .
    ebtx:
            [ebtx] Hello from EBT!
    compile:
            [compile] compiling ./src to /home/devel/projects/ebt/hello_ebt/out/production/hello-1.0.1/ebin
            [compile] compile src/hello.erl
            [compile] compiling ./test to /home/devel/projects/ebt/hello_ebt/out/test/hello-1.0.1/ebin
      ..........


For other tasks and thair configuration see edoc, ebts own build scripts,
example project and source codes.

Any help or suggestions will be appreciated.

Discussion group: https://groups.google.com/forum/#!forum/erlang-build-tool

