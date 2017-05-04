%%% @author Duncan Sparrell
%%% @copyright (C) 2017, sFractal Consulting LLC
%%%
-module(init_handler).
-author("Duncan Sparrell").
-license("Apache 2.0").

%%%-------------------------------------------------------------------
%%% All rights reserved.
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
%%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------

-export([init/3
        , rest_init/2
        , allowed_methods/2
        , content_types_accepted/2
        , handle_json/2
        ]).

-ignore_xref({init, 3}).
-ignore_xref({rest_init, 2}).
-ignore_xref({allowed_methods, 2}).
-ignore_xref({content_types_accepted, 2}).
-ignore_xref({handle_json, 2}).

init( {tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {Method, Req1} = cowboy_req:method(Req),
    {URL, Req2} = cowboy_req:url(Req1),
    lager:info("rest_init:~s ~s", [Method, URL]),

    %% initialize State with an empty pid map
    State = #{ pids => #{} },
    {ok, Req2, State}.

allowed_methods(Req, State) ->
    %%lager:info("got to allowed methods"),
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    %%lager:info("got to content_types"),
    %% header has content =application/json/whatever
    { [{ { <<"application">>, <<"json">>, '*'} , handle_json}], Req, State}.

handle_json(Req, State) ->
    %%lager:info("got to handle_json"),

    %% check for case of no body
    HasBody = cowboy_req:has_body(Req),
    body_check(HasBody, Req, State ).

%% handle case of whether body present or not
body_check(false, Req, State) ->
    %% no body so bad request
    State2 = maps:put(has_http_body, false, State),
    {ok, Req2} = cowboy_req:reply(400, [], <<"Missing body.">>, Req),
    %% return (don't move on since request was bad)
    {ok, Req2, State2};

body_check(true, Req, State) ->
    %% body present so move to next test
    State2 = maps:put(has_http_body, true, State),

    %% get the body of the request
    { ok, Body, Req1} = cowboy_req:body(Req),
    State3 = maps:put(http_body, Body, State2),

    %% check if body is json as it should be
    IsJson = jsx:is_json(Body),

    is_body_json(IsJson, Req1, State3).

is_body_json(false, Req, State) ->
    %% decoding json failed so bad request
    State2 = maps:put(good_json, false, State),
    {ok, Req2} = cowboy_req:reply(400, [], <<"Bad JSON">>, Req),
    %% return (don't move on since request was bad)
    %%   is this correct return tuple?
    {ok, Req2, State2};

is_body_json(true, Req, State) ->
    %% json decodes ok so move on to next test
    State2 = maps:put(good_json, true, State),

    Body = maps:get(http_body, State2),
    JsonMap = jsx:decode(Body, [return_maps]),
    lager:info("handle_json Json: InputMap ~p", [JsonMap] ),
    State3 = maps:put(json_map, JsonMap, State2),

    %% check for simulator_type
    SimulatorTypeExists = maps:is_key( <<"simulator_type">>, JsonMap ),
    %% tail recurse on to check simulator type
    has_sim( SimulatorTypeExists, JsonMap, Req, State3 ).

has_sim(false, _JsonMap, Req, State ) ->
    %% Json doesn't have simulator type so abort ie bad request
    State2 = maps:put(has_sim, false, State),
    {ok, Req2} = cowboy_req:reply(400, [], <<"Missing simulator type">>, Req),
    %% return (don't move on since request was bad)
    {ok, Req2, State2};

has_sim(true, JsonMap, Req, State ) ->
    %% json has simulator type so move on
    State2 = maps:put(has_sim, true, State),

    %% get the simulator type
    %%   already know it's there from hitting this function head
    SimTypeBin = maps:get( <<"simulator_type">>, JsonMap ),
    %%lager:info("SimType bintext: ~p", [SimTypeBin] ),

    %% react to which type of simulator
    init_sim(SimTypeBin, JsonMap, Req, State2).

init_sim(<<"language">>, JsonMap, Req, State) ->
    %% simulator type = language
    State2 = maps:put(has_valid_sim_type, true, State),
    State3 = maps:put(simulator_type, language, State2),

    %% get restart parameter if it was set (for use later)
    Restart = maps:get(<<"restart">>, JsonMap, false),
    lager:info("init_sim restart ~p", [Restart]),

    %% language simulator is stateless so simple init and move on
    %%    start env server as language simulator
    %%    see if env server already started
    Started = whereis(oc_env),
    case Started of
        undefined ->
            %% note restart in input is ignored if env not started
            %%      since will be setting to zero regardless
            lager:info("env not started prior"),
            %% not started yet, so start it
            %%     first prepare state for sim to preserve
            RestartCount = 0,       % since oc_env was not running
            SvrMap = #{ oc_env => true },      % since oc_env was not running
            StartState = begin_state( language
                                    , RestartCount
                                    , SvrMap
                                    ),
            {ok, _Pid} = oc_env:start(StartState),

            Headers = [ { <<"content-type">>
                        , <<"application/json">>
                        }
                      ],

            Status = oc_env:status(),
            lager:debug("Status ~p", [Status]),
            Body = jsx:encode(Status),

            %% respond that env has been (re)initialized to language
            {ok, Req2} = cowboy_req:reply(200
                                         , Headers
                                         , Body
                                         , Req
                                         ),
            %% finish
            {halt, Req2, State3};

        Started when is_pid(Started) ->
            lager:info("env was started prior"),
            %% already started - so reinitialize it as language simulator
            %% if needed get old state with reset count
            RestartCount = restart_count(Restart),

            %% stop all servers except env
            %%    for now, this trusts env knows the servers
            SvrMap = maps:get(svr_map, oc_env:status() ),
            EnvSvrMap = maps:remove(oc_env, SvrMap),
            EnvSvrList = maps:keys(EnvSvrMap),

            %% stop all servers
            oc_svr_stop:stop(EnvSvrList),

            NewSvrMap = #{ oc_env => true},   % start empty but env
            NewState = begin_state( language
                                    , RestartCount
                                    , NewSvrMap
                                    ),
            %% send message to env to restart as language
            oc_env:restart(NewState),

            Headers = [ { <<"content-type">>
                        , <<"application/json">>
                        }
                      ],

            Status = oc_env:status(),
            lager:debug("Status ~p", [Status]),
            Body = jsx:encode(Status),

            %% respond that env has been (re)initialized to language
            {ok, Req2} = cowboy_req:reply(200
                                         , Headers
                                         , Body
                                         , Req
                                         ),

            {halt, Req2, State3}
        end;

init_sim(<<"actuator">>, JsonMap, Req, State) ->
    %% simulator type = actuator,
    State2 = maps:put(has_valid_sim_type, true, State),
    State3 = maps:put(simulator_type, actuator, State2),

    %% actuator simulator needs inital state (eg which actuator)
    Need1 = "need to do actuator init from jsonmap",
    Need2 = io_lib:format("~p: ~p", [Need1, JsonMap] ),
    lager:error(Need2),

    %% respond that env has been (re)initialized to actuator
    ToDo01 = <<"Simulator init - actuator (not done yet)">>,
    {ok, Req2} = cowboy_req:reply(200
                                 , []
                                 , ToDo01
                                 , Req
                                 ),

    %% end
    {ok, Req2, State3};

init_sim(<<"orchestrator">>, JsonMap, Req, State) ->
    %% simulator type = orchestrator,
    State2 = maps:put(has_valid_sim_type, true, State),
    State3 = maps:put(simulator_type, orchestrator, State2),


    %% orchestrator simulator needs inital state
    Need1 = "need to do orchestrator init from jsonmap",
    Need2 = io_lib:format("~p: ~p", [Need1, JsonMap] ),
    lager:error(Need2),
    ToDo02 = <<"Simulator init - orchestrator (not done yet)">>,
    {ok, Req2} = cowboy_req:reply(200
                                 , []
                                 , ToDo02
                                 , Req
                                 ),



    %% finish
    {halt, Req2, State3};

init_sim(SimTypeBin, _JsonMap, Req, State) ->
    %% Didn't match simulator type
    lager:error("bad SimTypeBin: ~p", [SimTypeBin]),
    State2 = maps:put(has_valid_sim_type, false, State),
    {ok, Req2} = cowboy_req:reply(400, [], <<"Bad simulator type.">>, Req),
    %% return (don't move on since request was bad)
    {halt, Req2, State2}.

restart_count(<<"true">>) ->
    %% Restart = true so set count to zero
    0;

restart_count(_Restart) ->
    %% anything but true, don't reset count
    %% get count from env
    OldStatus = oc_env:status(),
    OldRestartCount = maps:get(restart_count, OldStatus),
    NewRestartCount = OldRestartCount + 1,
    NewRestartCount.

begin_state(language, RestartCount, SvrMap) ->
    %% initialize state of env server for simulator in language validation mode
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
    State.

