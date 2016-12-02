-module(targets).
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

-export([ get_target/3 ]).

get_target(false, Req, State ) ->
    %% HasTarget=false so move on
    %% reached here because no target info
    %%    this is an error
    lager:info("No Target"),
    {ok, Req2} = cowboy_req:reply( 400
                                 , []
                                 , <<"No Target Specified">>
                                 , Req
                                 ),
    %% don't continue on, return because of unexpected response
    {ok, Req2, State};

get_target(true, Req, State ) ->
    %% HasTarget=true so get process it

    %% get target info
    JsonMap = maps:get(json_map, State),

    TargetJson = maps:get(<<"target">>, JsonMap, target_undefined),
    lager:info("target json: ~p", [TargetJson]),

    %% get type of target
    TargetType = maps:get(<<"type">>, TargetJson, target_type_undefined),
    lager:info("target type: ~p", [TargetType]),

    %% recurse on and handle target type
    handle_target_type(TargetType, TargetJson, Req, State ).

handle_target_type(target_type_undefined, _TargetJson, Req, State ) ->
    %% oops - no type in target input
    lager:info("No Target type"),
    {ok, Req2} = cowboy_req:reply( 400
                                 , []
                                 , <<"No Target Type Specified">>
                                 , Req
                                 ),

    %% don't continue on, return because of unexpected response
    {ok, Req2, State};

handle_target_type(<<"cybox:hostname">>, TargetJson, Req, State ) ->
    %% target is a hostname
    State2 = maps:put(target_type, hostname, State),

    %% later on put this in hostname module to be more generic

    %% get specifiers
    Specifiers = maps:get(<<"specifiers">>, TargetJson),
    lager:info("target specifiers: ~p", [Specifiers] ),

    %% hostname type should have specifier of hostname_value
    HostName = maps:get(<<"hostname_value">>, Specifiers, hostname_undefined),
    lager:info("hostname: ~p", [HostName] ),

    %% spinup a server for this hostname
    %%   when get beyond one command, need to check first if already exists
    spawn_target( {hostname, HostName}, Req, State2 );

handle_target_type(<<"cybox:address">>, TargetJson, Req, State ) ->
    %% target is a address
    State2 = maps:put(target_type, address, State),

    %% later on put this in address module to be more generic

    %% get specifiers
    Specifiers = maps:get(<<"specifiers">>, TargetJson),
    lager:info("target specifiers: ~p", [Specifiers] ),

    AddressType = maps:get( <<"cybox:address_object_type">>
                          , Specifiers
                          , address_type_undefined
                          ),

    %% tail recurse on based on address type
    handle_address(AddressType, Specifiers, Req, State2 );

handle_target_type(<<"cybox:device">>, TargetJson, Req, State ) ->
    %% target is a device
    State2 = maps:put(target_type, device, State),

    %% later on put this in device module to be more generic

    %% get specifiers
    Specifiers = maps:get(<<"specifiers">>, TargetJson),
    lager:info("target specifiers: ~p", [Specifiers] ),

    %% handle which device it is
    handle_device( Specifiers , Req, State2);

handle_target_type(<<"command">>, TargetJson, Req, State ) ->
    lager:debug("need to put stuff here"),
    lager:debug("handle_target_type:command ~p", [TargetJson] ),
    {ok, Req, State};  % replace this

handle_target_type(TargetType, _TargetJson, Req, State ) ->
    %% ? dont know that TargetType
    lager:info("~p Don't know this Target type", [TargetType] ),
    {ok, Req2} = cowboy_req:reply( 400
                                 , []
                                 , <<"Don't know Target Type">>
                                 , Req
                                 ),

    %% don't continue on, return because of unexpected response
    {ok, Req2, State}.

handle_device( <<"network_firewall">>, Req, State) ->
    %%   when get beyond one command, need to check first if already exists
    spawn_target( {network_firewall, nonspecific} , Req, State );

handle_device( <<"network_scanner">>, Req, State) ->
    %%   when get beyond one command, need to check first if already exists
    spawn_target( {network_scanner, nonspecific} , Req, State );

handle_device( UnknownDevice, Req, State) ->
    %% ? dont know that Device
    lager:info("~p Don't know this Target cybox Device", [UnknownDevice] ),
    {ok, Req2} = cowboy_req:reply( 400
                                 , []
                                 , <<"Don't know cybox device">>
                                 , Req
                                 ),

    %% don't continue on, return because of unexpected response
    {ok, Req2, State}.

handle_address(<<"ipv4-addr">>, Specifiers, Req, State ) ->
    State2 = maps:put(target_address_type, ipv4, State),
    Ipv4Address = maps:get(<<"address-value">>, Specifiers, address_undefined),
    lager:debug("ipv4 ~p", [Ipv4Address] ),
    State3 = maps:put(target_address_value, Ipv4Address, State2),

    %% spinup a server for this address
    %%   when get beyond one command, need to check first if already exists
    spawn_target( {ipv4_address, Ipv4Address}, Req, State3 );

handle_address(<<"ipv6-addr">>, Specifiers, Req, State ) ->
    State2 = maps:put(target_address_type, ipv6, State),
    Ipv6Address = maps:get(<<"address-value">>, Specifiers, address_undefined),
    State3 = maps:put(target_address_value, Ipv6Address, State2),

    %% spinup a server for this address
    %%   when get beyond one command, need to check first if already exists
    spawn_target( {ipv6_address, Ipv6Address}, Req, State3 );

handle_address(AddressType, _Specifiers, Req, State ) ->
    %% unknown address type
    lager:info("~p Don't know this Address type", [AddressType] ),
    {ok, Req2} = cowboy_req:reply( 400
                                 , []
                                 , <<"Don't know Address Type">>
                                 , Req
                                 ),

    %% don't continue on, return because of unexpected response
    {ok, Req2, State}.

%% spawn target servers
spawn_target( {hostname, HostName}, Req, State ) ->
    State2 = maps:put(target, hostname, State),
    State3 = maps:put(hostname, HostName, State2),
    %% start gen_server for that target
    {ok, Pid} = tgt_hostname:start(State),
    State4 = tools:add_pid(tgt_hostname_pid, Pid, State3),

    %% check with keep alive
    TargetKeepAlive = tgt_hostname:keepalive(),
    lager:debug("TargetKeepAlive: ~p ", [TargetKeepAlive]),

    %% tail end recurse
    target_valid({hostname, HostName}, TargetKeepAlive, Req, State4);

spawn_target( {ipv4_address, Ipv4Address}, Req, State ) ->
    State2 = maps:put(target, ipv4_address, State),
    State3 = maps:put(ipv4_address, Ipv4Address, State2),
    %% start gen_server for that target
    {ok, Pid} = tgt_ipv4_address:start(State),
    State4 = tools:add_pid(tgt_ipv4_pid, Pid, State3),

    %% check with keep alive
    TargetKeepAlive = tgt_ipv4_address:keepalive(),
    lager:debug("TargetKeepAlive: ~p ", [TargetKeepAlive]),

    %% tail end recurse
    target_valid({ipv4_address, Ipv4Address}, TargetKeepAlive, Req, State4);

spawn_target( {ipv6_address, Ipv6Address}, Req, State ) ->
    State2 = maps:put(target, ipv6_address, State),
    State3 = maps:put(ipv6_address, Ipv6Address, State2),
    %% start gen_server for that target
    {ok, Pid} = tgt_ipv6_address:start(State),
    State4 = tools:add_pid(tgt_ipv6_pid, Pid, State3),

    %% check with keep alive
    TargetKeepAlive = tgt_ipv6_address:keepalive(),
    lager:debug("TargetKeepAlive: ~p ", [TargetKeepAlive]),

    %% tail end recurse
    target_valid({ipv6_address, Ipv6Address}, TargetKeepAlive, Req, State4);

spawn_target( {network_firewall, NetworkFirewall}, Req, State ) ->
    State2 = maps:put(target, network_firewall, State),
    State3 = maps:put(network_firewall, NetworkFirewall, State2),
    %% start gen_server for that target
    {ok, Pid} = tgt_network_firewall:start(State3),
    State4 = tools:add_pid(tgt_nfw_pid, Pid, State3),

    %% check with keep alive
    TargetKeepAlive = tgt_network_firewall:keepalive(),
    lager:debug("TargetKeepAlive: ~p ", [TargetKeepAlive]),

    %% tail end recurse
    target_valid( {network_firewall, NetworkFirewall}
                , TargetKeepAlive
                , Req
                , State4
                );

spawn_target( {network_scanner, NetworkScanner}, Req, State ) ->
    State2 = maps:put(target, network_scanner, State),
    State3 = maps:put(network_scanner, NetworkScanner, State2),
    %% start gen_server for that target
    {ok, Pid} = tgt_network_scanner:start(State3),
    State4 = tools:add_pid(tgt_scanner_pid, Pid, State3),

    %% check with keep alive
    TargetKeepAlive = tgt_network_scanner:keepalive(),
    lager:debug("TargetKeepAlive: ~p ", [TargetKeepAlive]),

    %% tail end recurse
    target_valid( {network_scanner, NetworkScanner}
                , TargetKeepAlive
                , Req
                , State4
                );

spawn_target( {TargetType, Value},  Req, State ) ->
    lager:info("spawn_target - no function head for ~p; Value=~p",
               [TargetType, Value]),
    %% no function for this target so reply accordingly
    {ok, Req2} = cowboy_req:reply( 400
                                 , []
                                 , <<"Missing target function">>
                                 , Req
                                 ),
    {ok, Req2, State}.

target_valid(_Target, TargetKeepAlive, Req, State) ->
    %% target was valid so update State
    State2 = maps:put(target_valid, true, State),

    %% tail end recurse to verifying keepalive
    verify_keepalive( TargetKeepAlive, Req, State2).

verify_keepalive( {keepalive_received, Server}
                , Req
                , State
                ) ->
    %% keepalive worked as expected
    State2 = maps:put(target_keepalive, true, State),
    State3 = maps:put(target_server, Server, State2),

    %% determine whether has actuator and tail recurse
    HasActuator = maps:get(has_actuator, State3),
    lager:debug("HasActuator: ~p", [HasActuator]),
    %% tail recurse to handling actuator
    actuators:get_actuator(HasActuator, Req, State3);

verify_keepalive( UnexpectedKeepalive
                , Req
                , State
                ) ->
    %% didnot get expected keepalive response
    State2 = maps:put(target_keepalive, false, State),
    lager:info("~p UnexpectedKeepalive", [UnexpectedKeepalive]),
    {ok, Req2} = cowboy_req:reply( 400
                                 , []
                                 , <<"keepalive failed">>
                                 , Req
                                 ),
    %% don't continue on, return because of unexpected response
    {ok, Req2, State2}.

