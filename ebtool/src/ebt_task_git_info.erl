%%  The MIT License (MIT)
%%
%%  Copyright (c) 2014 Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>
%%
%%  Permission is hereby granted, free of charge, to any person obtaining a copy of
%%  this software and associated documentation files (the "Software"), to deal in
%%  the Software without restriction, including without limitation the rights to
%%  use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
%%  the Software, and to permit persons to whom the Software is furnished to do so,
%%  subject to the following conditions:
%%
%%  The above copyright notice and this permission notice shall be included in all
%%  copies or substantial portions of the Software.
%%
%%  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
%%  FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
%%  COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
%%  IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%%  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

-module(ebt_task_git_info).

-export([perform/3]).

-define(cmds, [
    {repository, "echo -n `git config --get remote.origin.url`"},
    {branch, "echo -n `git rev-parse --abbrev-ref HEAD`"},
    {commit, "git --no-pager log -1 --pretty='format:%H'"}
]).

perform(_Target, _Dir, Config) ->
    GitInfo = lists:map(fun({Key, Command}) ->
        case xl_shell:command(Command) of
            {ok, Data} ->
                io:format("~s: ~s~n", [Key, Data]),
                {Key, Data};
            {error, E} ->
                io:format("git command failed: ~n~s~n~p~n", [Command, E]),
                {Key, undefined}
        end
    end, ?cmds),
    {ok, ebt_config:update_buildinfo(Config, git, GitInfo)}.

