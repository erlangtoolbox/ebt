%%  Copyright (c) 2012-2013 StrikeAd LLC http://www.strikead.com
%%  Copyright (c) 2012-2014 Vladimir Kirichenko vladimir.kirichenko@gmail.com
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
%%      Neither the name of the EBT nor the names of its
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
{application, ebt, [
    {description, "Erlang Build Tool"},
    {registered, []},
    {applications, [
        kernel,
        stdlib,
        ebt__xl_stdlib,
        ebt__getopt,
        ebt__erlandox
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
                {patchwork, ebt_task_graphviz}
            ]},
            {targets, [
                {dialyze, [build_plt, compile]},
                {compile, [template, leex, yecc, protoc, cc]},
                {eunit, [compile, cover]},
                {cover_analyse, [eunit]},
                {edoc, [dot, neato, twopi, circo, fdp, sfdp, patchwork]},
                {package, [cover_analyse, edoc, git_info]},
                {escript, [cover_analyse]}
            ]}
        ]}
    ]}
]}.
