%% The MIT License (MIT)
%%
%% Copyright (c) 2014 Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy of
%% this software and associated documentation files (the "Software"), to deal in
%% the Software without restriction, including without limitation the rights to
%% use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
%% the Software, and to permit persons to whom the Software is furnished to do so,
%% subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
%% FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
%% COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
%% IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
{application, ebtool, [
    {description, "Erlang Build Tool"},
    {registered, []},
    {applications, [
        kernel,
        stdlib,
        xl_stdlib,
        getopt,
        et_code,
        et_lang
    ]},
    {env, [
        {tasks, [
            {modules, [
                {clean, ebt_task_clean},
                {leex, ebt_task_leex},
                {yecc, ebt_task_yecc},
                {compile, ebt_task_compile},
                {depends, ebt_task_depends},
                {release, ebt_task_release},
                {eunit, ebt_task_eunit},
                {package, ebt_task_package},
                {escript, ebt_task_escript},
                {protoc, ebt_task_protoc},
                {shell, ebt_task_shell},
                {template, ebt_task_template},
                {git_info, ebt_task_git_info},
                {edoc, ebt_task_edoc},
                {build_plt, ebt_task_build_plt},
                {dialyze, ebt_task_dialyze},
                {cc, ebt_task_cc},
                {cover, ebt_task_cover},
                {cover_analyse, ebt_task_cover_analyse},
                {dot, ebt_task_graphviz},
                {neato, ebt_task_graphviz},
                {twopi, ebt_task_graphviz},
                {circo, ebt_task_graphviz},
                {fdp, ebt_task_graphviz},
                {sfdp, ebt_task_graphviz},
                {patchwork, ebt_task_graphviz},
                {info, ebt_task_info},
                {code_escape, ebt_task_code_escape},
                {fpm, ebt_task_fpm}
            ]},
            {targets, [
                {code_escape, [compile]},
                {dialyze, [build_plt, compile]},
                {compile, [template, leex, yecc, protoc, cc]},
                {eunit, [compile, cover]},
                {cover_analyse, [eunit]},
                {edoc, [dot, neato, twopi, circo, fdp, sfdp, patchwork]},
                {info, [git_info]},
                {package, [cover_analyse, edoc, info]},
                {escript, [cover_analyse]}
            ]}
        ]}
    ]}
]}.
