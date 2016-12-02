-module(actuators).
-author("Duncan Sparrell").
-license("Apache 2.0").
%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2016, sFractal Consulting LLC
%%%
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
%%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES. LOSS OF USE,
%%% DATA, OR PROFITS. OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------

-export([ get_actuator/3 ]).

get_actuator(false, Req, State ) ->
    %% No actuator specified so assume domain is domain of receiver
    ActuatorType = demense,
    lager:info("actuator type: ~p", [ActuatorType]),
    %% instead of handle_actuator, do demense special case here

    spawn_actuator( {demense, all}, Req, State );

get_actuator(true, Req, State ) ->
    %% HasActuator=true so get process it

    %% get actuator info
    JsonMap = maps:get(json_map, State),

    ActuatorJson = maps:get(<<"actuator">>, JsonMap, actuator_undefined),
    lager:info("actuator json: ~p", [ActuatorJson]),

    %% get type of actuator
    ActuatorType = maps:get(<<"type">>, ActuatorJson, actuator_type_undefined),
    lager:info("actuator type: ~p", [ActuatorType]),

    %% recurse on and handle actuator type
    handle_actuator_type(ActuatorType, ActuatorJson, Req, State ).

handle_actuator_type(actuator_type_undefined, _ActuatorJson, Req, State ) ->
    %% oops - no type in actuator input
    lager:info("No Actuator type"),
    {ok, Req2} = cowboy_req:reply( 400
                                 , []
                                 , <<"No Actuator Type Specified">>
                                 , Req
                                 ),

    %% don't continue on, return because of unexpected response
    {ok, Req2, State};

handle_actuator_type(<<"network-firewall">>, ActuatorJson, Req, State ) ->
    %% actuator is a network-firewall
    State2 = maps:put(actuator_type, network_firewall, State),

    %% later on put this in network firewall module to be more generic

    %% get specifier
    Specifiers = maps:get(<<"specifiers">>, ActuatorJson),
    lager:info("actuator specifiers: ~p", [Specifiers] ),

    %% in this case specifier is name of firewall

    %% spinup a server for this hostname
    %%   when get beyond one command, need to check first if already exists
    spawn_actuator( {network_firewall, Specifiers}, Req, State2 );

handle_actuator_type(<<"network-router">>, ActuatorJson, Req, State ) ->
    %% actuator is a network router
    State2 = maps:put(actuator_type, network_router, State),

    %% later on put this in router module to be more generic

    %% get specifiers
    Specifiers = maps:get(<<"specifiers">>, ActuatorJson),
    lager:info("actuator specifiers: ~p", [Specifiers] ),

    %% in this case specifier is name of router

    %% spinup a server for this router
    %%   when get beyond one command, need to check first if already exists
    spawn_actuator( {network_router, Specifiers}, Req, State2 );

handle_actuator_type(<<"network-scanner">>, ActuatorJson, Req, State ) ->
    %% actuator is a network-scanner
    State2 = maps:put(actuator_type, network_scanner, State),

    %% later on put this in network_scanner module to be more generic

    %% get specifiers
    Specifiers = maps:get(<<"specifiers">>, ActuatorJson),
    lager:info("actuator specifiers: ~p", [Specifiers] ),

    %% in this case specifier is name of scanner

    %% spinup a server for this router
    %%   when get beyond one command, need to check first if already exists
    spawn_actuator( {network_scanner, Specifiers}, Req, State2 );

handle_actuator_type(ActuatorType, _ActuatorJson, Req, State ) ->
    %% ? dont know that ActuatorType
    lager:info("~p Don't know this Actuator type", [ActuatorType] ),
    {ok, Req2} = cowboy_req:reply( 400
                                 , []
                                 , <<"Don't know Actuator Type">>
                                 , Req
                                 ),

    %% don't continue on, return because of unexpected response
    {ok, Req2, State}.

%% spawn actuator servers

spawn_actuator( {demense, all}, Req, State ) ->
    State2 = maps:put(actuator, demense, State),
    %% start gen_server for that actuator
    {ok, Pid} = acu_demense:start(State),
    State3 = tools:add_pid(acu_demense_pid, Pid, State2),

    %% check with keep alive
    ActuatorKeepAlive = acu_demense:keepalive(),
    lager:debug("ActuatorKeepAlive: ~p ", [ActuatorKeepAlive]),

    %% tail end recurse
    actuator_valid( {demense, all}
                , ActuatorKeepAlive
                , Req
                , State3
                );

spawn_actuator( {network_firewall, NetworkFirewall}, Req, State ) ->
    State2 = maps:put(actuator, network_firewall, State),
    State3 = maps:put(network_firewall, NetworkFirewall, State2),
    %% start gen_server for that actuator
    {ok, Pid} = acu_network_firewall:start(State),
    State4 = tools:add_pid(acu_nfw_pid, Pid, State3),

    %% check with keep alive
    ActuatorKeepAlive = acu_network_firewall:keepalive(),
    lager:debug("ActuatorKeepAlive: ~p ", [ActuatorKeepAlive]),

    %% tail end recurse
    actuator_valid( {network_firewall, NetworkFirewall}
                , ActuatorKeepAlive
                , Req
                , State4
                );

spawn_actuator( {network_router, NetworkRouter}, Req, State ) ->
    State2 = maps:put(actuator, network_router, State),
    State3 = maps:put(network_router, NetworkRouter, State2),
    %% start gen_server for that actuator
    {ok, Pid} = acu_network_router:start(State),
    State4 = tools:add_pid(acu_rtr_pid, Pid, State3),

    %% check with keep alive
    ActuatorKeepAlive = acu_network_router:keepalive(),
    lager:debug("ActuatorKeepAlive: ~p ", [ActuatorKeepAlive]),

    %% tail end recurse
    actuator_valid( {network_router, NetworkRouter}
                , ActuatorKeepAlive
                , Req
                , State4
                );

spawn_actuator( {network_scanner, NetworkScanner}, Req, State ) ->
    State2 = maps:put(actuator, network_scanner, State),
    State3 = maps:put(network_scanner, NetworkScanner, State2),
    %% start gen_server for that actuator
    {ok, Pid} = acu_network_scanner:start(State),
    State4 = tools:add_pid(acu_scanner_pid, Pid, State3),

    %% check with keep alive
    ActuatorKeepAlive = acu_network_scanner:keepalive(),
    lager:debug("ActuatorKeepAlive: ~p ", [ActuatorKeepAlive]),

    %% tail end recurse
    actuator_valid( {network_scanner, NetworkScanner}
                , ActuatorKeepAlive
                , Req
                , State4
                );

spawn_actuator( {ActuatorType, Value},  Req, State ) ->
    lager:info("spawn_actuator - no function head for ~p; Value=~p",
               [ActuatorType, Value]),
    %% no function for this actuator so reply accordingly
    {ok, Req2} = cowboy_req:reply( 400
                                 , []
                                 , <<"Missing actuator function">>
                                 , Req
                                 ),
    {ok, Req2, State}.

actuator_valid(_Actuator, ActuatorKeepAlive, Req, State) ->
    %% actuator was valid so update State
    State2 = maps:put(actuator_valid, true, State),

    %% tail end recurse to verifying keepalive
    verify_keepalive( ActuatorKeepAlive, Req, State2).

verify_keepalive( {keepalive_received, Server}
                , Req
                , State
                ) ->
    %% keepalive worked as expected
    State2 = maps:put(actuator_keepalive, true, State),
    State3 = maps:put(actuator_server, Server, State2),

    %% tail recurse to handling modifiers
    %%   after gettin whether there are any modifiers
    HasModifers = maps:get(has_modifiers, State3),
    modifiers:get_modifiers( HasModifers, Req, State3);


verify_keepalive( UnexpectedKeepalive
                , Req
                , State
                ) ->
    %% didnot get expected keepalive response
    State2 = maps:put(actuator_keepalive, false, State),
    lager:info("~p UnexpectedKeepalive", [UnexpectedKeepalive]),
    {ok, Req2} = cowboy_req:reply( 400
                                 , []
                                 , <<"keepalive failed">>
                                 , Req
                                 ),
    %% don't continue on, return because of unexpected response
    {ok, Req2, State2}.

