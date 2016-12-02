-module(modifiers).
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

-export([ get_modifiers/3 ]).

%% unlike actions/targets (of which there must be 1 and only 1)
%%  or actuators (or which there may 0 or 1);
%% there may be zero to many modifiers

get_modifiers(false, Req, State ) ->
    %% No modifiers specified so go on to response with no ack required
    lager:info("no modifiers"),

    send_response:send_response(Req, State);

get_modifiers(true, Req, State ) ->
    %% HasModifier=true so get modifier(s) and  process it(them)

    %% get modifier(s) info
    JsonMap = maps:get(json_map, State),

    ModifiersJson = maps:get(<<"modifiers">>, JsonMap, modifiers_undefined),
    lager:info("modifier json: ~p", [ModifiersJson]),

    %% should be a map, make a key list to recurse thru
    Keys = maps:keys(ModifiersJson),

    %% recurse thru the modifiers
    handle_modifiers( Keys, ModifiersJson, Req, State ).

handle_modifiers( [], _ModifiersJson, Req, State ) ->
    %% key list empty so done recursing, move on to send response
    send_response:send_response(Req, State);

handle_modifiers( [NextKey | RestOfKeys ], ModifiersJson, Req, State ) ->
    %% handle the next modifier
    Value = maps:get(NextKey, ModifiersJson, undefined),

    % process the modifier
    State2 = process_modifier( NextKey, Value, Req, State),

    % recurse to next modifier
    handle_modifiers( RestOfKeys, ModifiersJson, Req, State2 );

handle_modifiers( _Keys, _ModifiersJson, Req, State ) ->
    %% didn't match? handle error
    lager:info("bad match on modifiers"),
    {ok, Req2} = cowboy_req:reply( 400
                                 , []
                                 , <<"Problem with modifiers">>
                                 , Req
                                 ),

    %% don't continue on, return because of unexpected response
    {ok, Req2, State}.

process_modifier( <<"response">>, Value, _Req, State) ->
    State2 = maps:put(response, Value, State),
    %% start gen_server for modifier
    {ok, Pid} = mod_ack:start(State),
    State3 = tools:add_pid(mod_response_pid, Pid, State2),

    %% check with keep alive
    { keepalive_received, Server } = mod_ack:keepalive(),
    lager:debug("ModifierKeepAlive: ~p ", [Server]),

    State4 = maps:put(response_keepalive, Server, State3),

    %% return state and go try another modifier
    State4;

process_modifier( <<"where">>, Value, _Req, State) ->
    State2 = maps:put(where, Value, State),
    %% start gen_server for modifier
    {ok, Pid} = mod_where:start(State),
    State3 = tools:add_pid(mod_where_pid, Pid, State2),

    %% check with keep alive
    { keepalive_received, Server } = mod_where:keepalive(),
    lager:debug("ModifierKeepAlive: ~p ", [Server]),

    State4 = maps:put(where_keepalive, Server, State3),

    %% return state and go try another modifier
    State4;

process_modifier( <<"id">>, Value, _Req, State) ->
    State2 = maps:put(id, Value, State),
    %% start gen_server for modifier
    {ok, Pid} = mod_id:start(State),
    State3 = tools:add_pid(mod_id_pid, Pid, State2),

    %% check with keep alive
    { keepalive_received, Server } = mod_id:keepalive(),
    lager:debug("ModifierKeepAlive: ~p ", [Server]),

    State4 = maps:put(id_keepalive, Server, State3),

    %% return state and go try another modifier
    State4;

process_modifier( <<"delay">>, Value, _Req, State) ->
    State2 = maps:put(delay, Value, State),
    %% start gen_server for modifier
    {ok, Pid} = mod_delay:start(State),
    State3 = tools:add_pid(mod_delay_pid, Pid, State2),

    %% check with keep alive
    { keepalive_received, Server } = mod_delay:keepalive(),
    lager:debug("ModifierKeepAlive: ~p ", [Server]),

    State4 = maps:put(delay_keepalive, Server, State3),

    %% return state and go try another modifier
    State4;

process_modifier( <<"duration">>, Value, _Req, State) ->
    State2 = maps:put(duration, Value, State),
    %% start gen_server for modifier
    {ok, Pid} = mod_duration:start(State),
    State3 = tools:add_pid(mod_duration_pid, Pid, State2),

    %% check with keep alive
    { keepalive_received, Server } = mod_duration:keepalive(),
    lager:debug("ModifierKeepAlive: ~p ", [Server]),

    State4 = maps:put(duration_keepalive, Server, State3),

    %% return state and go try another modifier
    State4;

process_modifier( <<"date_time">>, Value, _Req, State) ->
    State2 = maps:put(date_time, Value, State),
    %% start gen_server for modifier
    {ok, Pid} = mod_date_time:start(State),
    State3 = tools:add_pid(mod_date_time_pid, Pid, State2),

    %% check with keep alive
    { keepalive_received, Server } = mod_date_time:keepalive(),
    lager:debug("ModifierKeepAlive: ~p ", [Server]),

    State4 = maps:put(date_time_keepalive, Server, State3),

    %% return state and go try another modifier
    State4;

process_modifier( Modifier, Value, Req, State) ->
    %% oops, don't recognize
    lager:info("Don't recognize Modifier=~p, Value=~p", [Modifier, Value]),

    {ok, Req2} = cowboy_req:reply( 400
                                 , []
                                 , <<"Problem with modifiers">>
                                 , Req
                                 ),

    %% don't continue on, return because of unexpected response
    {ok, Req2, State}.

