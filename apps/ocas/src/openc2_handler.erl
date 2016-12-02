-module(openc2_handler).
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
    lager:info("got to allowed methods"),
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    lager:info("got to content_types"),
    %% header has content =application/json/whatever
    { [{ { <<"application">>, <<"json">>, '*'} , handle_json}], Req, State}.

handle_json(Req, State) ->
    lager:info("got to handle_json"),

    %% check for case of no body
    HasBody = cowboy_req:has_body(Req),
    body_check(HasBody, Req, State ).

%% handle case of whether body present or not
body_check(false, Req, State) ->
    %% no body so bad request
    State2 = maps:put(has_http_body, false, State),
    {ok, Req2} = cowboy_req:reply(400, [], <<"Missing body.">>, Req),
    %% return (don't move on since request was bad)
    %%   is this correct return tuple?
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

    %% check for action
    ActionKeyExists = maps:is_key( <<"action">>, JsonMap ),
    has_action( ActionKeyExists, Req, State3 ).


has_action(false, Req, State ) ->
    %% Json doesn't have a action so abort ie bad request
    State2 = maps:put(has_action, false, State),
    {ok, Req2} = cowboy_req:reply(400, [], <<"Missing action">>, Req),
    %% return (don't move on since request was bad)
    %%   is this correct return tuple?
    {ok, Req2, State2};

has_action(true, Req, State ) ->
    %% json has action so move on
    State2 = maps:put(has_action, true, State),

    %%   check existence of target, actuator, modifiers
    %%           for now just add them to State
    %%           figure out how to deal with them later

    JsonMap = maps:get(json_map, State2),
    TargetKeyExists = maps:is_key( <<"target">>, JsonMap ),
    State3 = maps:put(has_target, TargetKeyExists, State2),
    ActuatorKeyExists = maps:is_key( <<"actuator">>, JsonMap ),
    State4 = maps:put(has_actuator, ActuatorKeyExists, State3),
    ModifiersKeyExists = maps:is_key( <<"modifiers">>, JsonMap ),
    State5 = maps:put(has_modifiers, ModifiersKeyExists, State4),

    %% move on to next step - is it a valid action
    ActionBin = maps:get( <<"action">>, JsonMap ),
    lager:info("action bintext: ~p", [ActionBin] ),

    actions:spawn_action(ActionBin, Req, State5).
