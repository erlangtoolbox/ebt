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
    <<Prefix/binary, IoList/binary>>.

format_mfa(IoContext, M, F, A) -> format_context(IoContext, apply(M, F, A)).