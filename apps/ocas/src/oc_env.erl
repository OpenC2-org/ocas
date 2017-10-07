%%% @author Duncan Sparrell
%%% @copyright (C) 2017, sFractal Consulting LLC
%%%
-module(oc_env).
%%%-------------------------------------------------------------------
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
        , terminate/0
        , keepalive/0
        , status/0
        , restart/1
        , first_start/0
        , server_up/1
        , start_server/2
        , update_server/2
        ]).

-callback start(term()) -> {ok, pid()}.  % for handle_call  start_server

-ignore_xref({update_server, 2}). % to keep xref happy

%% This is the api to the server

start(State) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [State], []).

first_start() ->
    %% initialize state of env server for simulator in language validation mode
    RestartCount = 0,
    SvrMap = #{ oc_env => true },
    SimType = language,
    StartTime = erlang:timestamp(),
    StartTimeTuple = calendar:now_to_datetime(StartTime),
    {{Year, Month, Day}, {Hour, Minute, Second}} = StartTimeTuple,
    ReadableStartTime = #{ year => Year
                         , month => Month
                         , day => Day
                         , hour => Hour
                         , minute => Minute
                         , second => Second
                         },
    ThisMachine = list_to_binary( net_adm:localhost() ),
    InitState = #{},
    State = #{ simulator_type => SimType
             , restart_count => RestartCount
             , start_time => ReadableStartTime
             , this_machine => ThisMachine
             , init_state => InitState
             , svr_map => SvrMap
             },
    start(State).


stop() ->
    %% cast (in contrat to terminate which uses call)
    gen_server:cast(?MODULE, shutdown).

terminate() ->
    %% call (in contrat to stop which uses cast)
    gen_server:call(?MODULE, terminate).

keepalive() ->
    gen_server:call(?MODULE, keepalive).

status() ->
    gen_server:call(?MODULE, status).

restart(NewState) ->
    gen_server:call(?MODULE, {restart, NewState}).

server_up(Svr) ->
    gen_server:call(?MODULE, {server_up, Svr}).

start_server(Svr, State) ->
    lager:info("oc_env:start_server, Svr=~p, State=~p", [Svr, State]),
    gen_server:call(?MODULE, {start_server, {Svr, State} }).

update_server(Svr, State) ->
    gen_server:call(?MODULE, {update_server, {Svr, State} }).

%% initialize server with state
init( [State] ) ->
    lager:debug( "starting ~p with ~p", [?MODULE, State] ),
    { ok, State }.

%% synchronous calls
handle_call( keepalive, From, State ) ->
    lager:debug( "~p got keepalive from ~p", [?MODULE, From] ),
    %% reply to keepalive
    Response = {keepalive_received, tgt_hostname_server},
    {reply, Response, State};

handle_call( status, From, State ) ->
    lager:debug( "~p status request from ~p", [?MODULE, From] ),
    %% reply to status request with state
    Response = State,
    {reply, Response, State};

handle_call( {restart, NewState} , From, _State ) ->
    lager:debug( "~p restart with ~p from ~p", [?MODULE, NewState, From] ),
    %% set State to NewState and respond with that new state
    Response = NewState,
    {reply, Response, NewState};

handle_call(terminate, _From, State) ->
    lager:info( "~p got terminate", [?MODULE] ),
    {stop, normal, ok, State};

handle_call( {server_up, Svr} , _From, State ) ->
    lager:info( "~p got server_up ~p", [?MODULE, Svr] ),
    %% get existing server map
    SvrMapIn = maps:get(svr_map, State),
    %% add Svr to server map
    NewSvrMap = maps:put(Svr, true, SvrMapIn),
    %% take old svr map out of state and put new on in
    State2 = maps:remove(svr_map, State),
    NewState = maps:put(svr_map, NewSvrMap, State2),
    %% reply with ok and new state
    {reply, ok, NewState};

handle_call( {start_server, {Svr, IncomingState}}, _From, State ) ->
    %% start server if not already started
    {ok, SvrPid} = Svr:start(IncomingState),
    %% add server to svr map if not already there
    NewState = add_to_svr_map(Svr, SvrPid, State),
    {reply, ok, NewState};

handle_call( {update_server, {Svr, IncomingState} }, _From, State ) ->
    %% need to add code to put server in NewState
    lager:info("fix oc_env:handle_call:update_server ~p", [IncomingState]),
    NewState = maps:put(something, Svr, State),
    {reply, ok, NewState};

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

%% routine to update svr map in state when another svr added
add_to_svr_map(Svr, SvrPid, State) ->
    lager:info( "~p:add_to_svr_map got Svr= ~p", [?MODULE, Svr] ),
    lager:info( "add_to_svr_map got State= ~p", [State] ),
    %% get existing server map
    SvrMapIn = maps:get(svr_map, State),
    lager:info( "add_to_svr_map SvrMapIn= ~p", [SvrMapIn] ),
    %% add Svr to server map
    NewSvrMap = maps:put(Svr, SvrPid, SvrMapIn),
    lager:info( "NewSvrMap= ~p", [NewSvrMap] ),
    %% replace svr_map with NewSvrMap
    NewState = maps:update(svr_map, NewSvrMap, State),
    lager:info( "NewState= ~p", [NewState] ),
    %% reply with ok and new state
    % return new state
    NewState.
