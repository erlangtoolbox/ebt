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

%%%-------------------------------------------------------------------
%%% @author Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ebt_task_protoc_tests).
-author("Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>").

-include_lib("eunit/include/eunit.hrl").

basho_test() ->
    et_file:delete("/tmp/test"),
    et_file:mkdirs("/tmp/test/ebin"),
    et_file:mkdirs("/tmp/test/include"),
    ?assertEqual(ok, ebt_task_protoc:compile(
        basho,
        "test/addressbook.proto",
        "/tmp/test/ebin",
        "/tmp/test/src",
        "/tmp/test/include"
    )).

gpb_test() ->
    et_file:delete("/tmp/test"),
    et_file:mkdirs("/tmp/test/src"),
    et_file:mkdirs("/tmp/test/include"),
    ?assertEqual(ok, ebt_task_protoc:compile(
        gpb,
        "test/addressbook.proto",
        "/tmp/test/ebin",
        "/tmp/test/src",
        "/tmp/test/include"
    )).