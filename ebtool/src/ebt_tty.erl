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
%%% @author razer
%%% @doc
%%%
%%% @end
%%% Created : 23. Jan 2014 12:48 AM
%%%-------------------------------------------------------------------
-module(ebt_tty).
-author("Volodymyr Kyrychenko <vladimirk.kirichenko@gmail.com>").

%% API
-export([format/2, format/3, initialize/0, io_context/1, format_mfa/4, format/1]).

format(Message) ->
    format(user, Message, []).

format(Pattern, Args) ->
    format(user, Pattern, Args).

format(Device, Pattern, Args) ->
    io:format(Device, Pattern, Args).

initialize() ->
    MasterGroupLeader = group_leader(),
    IoLeader = spawn(fun() -> process_io(MasterGroupLeader, undefined) end),
    ebt_error_logger_tty:add_handler(IoLeader),
    put(io_leader, IoLeader),
    group_leader(IoLeader, self()).

io_context(Target) ->
    get(io_leader) ! {ebt_io_context, Target},
    ok.

process_io(MasterGroupLeader, IoContext) ->
    receive
        {ebt_io_context, Context} ->
            process_io(MasterGroupLeader, Context);
        Msg when IoContext == undefined ->
            MasterGroupLeader ! Msg,
            process_io(MasterGroupLeader, IoContext);
        {io_request, From, ReplyAs, {put_chars, Enc, IoList}} when is_binary(IoList) ->
            MasterGroupLeader ! {io_request, From, ReplyAs, {put_chars, Enc, format_context(IoContext, IoList)}},
            process_io(MasterGroupLeader, IoContext);
        {io_request, From, ReplyAs, {put_chars, Enc, M, F, A}} ->
            MasterGroupLeader ! {io_request, From, ReplyAs, {put_chars, Enc, ?MODULE, format_mfa, [IoContext, M, F, A]}},
            process_io(MasterGroupLeader, IoContext);
        Msg ->
            MasterGroupLeader ! Msg,
            process_io(MasterGroupLeader, IoContext)
    end.

format_context(IoContext, IoList) when is_list(IoList) -> format_context(IoContext, list_to_binary(IoList));
format_context(IoContext, IoList) when is_binary(IoList) ->
    Prefix = list_to_binary(io_lib:format("\t[~s] ", [IoContext])),
    Lines = [<<Prefix/binary, Line/binary, "\n">> || Line <- binary:split(IoList, <<"\n">>, [global, trim])],
    xl_string:join(Lines, <<>>).

format_mfa(IoContext, M, F, A) -> format_context(IoContext, apply(M, F, A)).