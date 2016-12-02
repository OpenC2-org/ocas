-module(act_distill).
%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2016, sFractal Consulting LLC
%%%
%%% All rights reserved.
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are
%%% met:
%%%
%%% * Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer.
%%%
%%% * Redistributions in binary form must reproduce the above copyright
%%%   notice, this list of conditions and the following disclaimer in the
%%%   documentation and/or other materials provided with the distribution.
%%%
%%% * The names of its contributors may not be used to endorse or promote
%%%   products derived from this software without specific prior written
%%%   permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES. LOSS OF USE,
%%% DATA, OR PROFITS. OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------

-behaviour(gen_server).

-author("Duncan Sparrell").
-license("Apache 2.0").

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

%% interface calls
-export([ start/1
        , stop/0
        , keepalive/0
        ]).

%% This routine API handles all the actions that can be taken

start(State) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [State], []).

stop() ->
    gen_server:cast(?MODULE, shutdown).

keepalive() ->
    gen_server:call(?MODULE, keepalive).

%% initialize server with state
init( [State] ) ->
    lager:debug( "starting ~p with ~p", [?MODULE, State] ),
    { ok, State }.

%% synchronous calls
handle_call( keepalive, From, State ) ->
    lager:debug( "~p got keepalive from ~p", [?MODULE, From] ),
    %% reply to keepalive
    Response = {keepalive_received, distill_server},
    {reply, Response, State};

%% handle unknown call messages
handle_call(Message, From, State) ->
    lager:info( "~p got unknown ~p from ~p", [?MODULE, Message, From] ),
    {reply, error, State}.

%% async calls
handle_cast(shutdown, State) ->
    lager:info( "~p got shutdown", [?MODULE] ),
    {stop, normal, State};

%% handle unknown cast messages
handle_cast(Message, State) ->
    lager:info( "~p got unknown ~p", [?MODULE, Message] ),
    {noreply, State}.

%% handle unknown info messages
handle_info(Message, State) ->
    lager:info( "~p got unknown ~p", [?MODULE, Message] ),
    {noreply, State}.

%% handle terminate
terminate(normal, _State) ->
    ok.

%% don't really handle code change yet
code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for behaviour sanity,
    %% but will not be used. Only a version on the next
    {ok, State}.
