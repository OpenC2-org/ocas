-module(actions).
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

-export([ spawn_action/3 ]).

%% software design note:
%%      spawn_action was consciously not simplified to prevent
%%      passing a routine based on input (avoids exploits and elvis complaints)

%% spawn action servers
spawn_action( <<"allow">>,  Req, State ) ->

    %% see if server already started
    Started = whereis(act_allow),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, Pid} = act_allow:start(State);
        Started when is_pid(Started) ->
            %% already started
            Pid = Started
    end,

    %% check with keep alive
    ActionKeepAlive = act_allow:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(allow, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"augment">>,  Req, State ) ->
    %% see if server already started
    Started = whereis(act_augment),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, Pid} = act_augment:start(State);
        Started when is_pid(Started) ->
            %% already started
            Pid = Started
    end,

    %% check with keep alive
    ActionKeepAlive = act_augment:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(augment, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"cancel">>,  Req, State ) ->
    %% see if server already started
    Started = whereis(act_cancel),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, Pid} = act_cancel:start(State);
        Started when is_pid(Started) ->
            %% already started
            Pid = Started
    end,
    %% check with keep alive
    ActionKeepAlive = act_cancel:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(cancel, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"contain">>,  Req, State ) ->
    %% start gen_server for that action
    {ok, Pid} = act_contain:start(State),

    %% check with keep alive
    ActionKeepAlive = act_contain:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(contain, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"copy">>,  Req, State ) ->
    %% start gen_server for that action
    {ok, Pid} = act_copy:start(State),

    %% check with keep alive
    ActionKeepAlive = act_copy:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(copy, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"delay">>,  Req, State ) ->
    %% start gen_server for that action
    {ok, Pid} = act_delay:start(State),

    %% check with keep alive
    ActionKeepAlive = act_delay:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(delay, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"delete">>,  Req, State ) ->
    %% see if server already started
    Started = whereis(act_delete),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, Pid} = act_delete:start(State);
        Started when is_pid(Started) ->
            %% already started
            Pid = Started
    end,

    %% check with keep alive
    ActionKeepAlive = act_delete:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(delete, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"deny">>,  Req, State ) ->
    %% see if server already started
    Started = whereis(act_deny),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, Pid} = act_deny:start(State);
        Started when is_pid(Started) ->
            %% already started
            Pid = Started
    end,

    %% check with keep alive
    ActionKeepAlive = act_deny:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(deny, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"detonate">>,  Req, State ) ->
    %% start gen_server for that action
    {ok, Pid} = act_detonate:start(State),

    %% check with keep alive
    ActionKeepAlive = act_detonate:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(detonate, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"distill">>,  Req, State ) ->
    %% start gen_server for that action
    {ok, Pid} = act_distill:start(State),

    %% check with keep alive
    ActionKeepAlive = act_distill:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(distill, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"get">>,  Req, State ) ->
    %% start gen_server for that action
    {ok, Pid} = act_get:start(State),

    %% check with keep alive
    ActionKeepAlive = act_get:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(get, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"investigate">>,  Req, State ) ->
    %% start gen_server for that action
    {ok, Pid} = act_investigate:start(State),

    %% check with keep alive
    ActionKeepAlive = act_investigate:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(investigate, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"locate">>,  Req, State ) ->
    %% start gen_server for that action
    {ok, Pid} = act_locate:start(State),

    %% check with keep alive
    ActionKeepAlive = act_locate:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(locate, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"mitigate">>,  Req, State ) ->
    %% see if server already started
    Started = whereis(act_mitigate),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, Pid} = act_mitigate:start(State);
        Started when is_pid(Started) ->
            %% already started
            Pid = Started
    end,

    %% check with keep alive
    ActionKeepAlive = act_mitigate:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(mitigate, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"modify">>,  Req, State ) ->
    %% start gen_server for that action
    {ok, Pid} = act_modify:start(State),

    %% check with keep alive
    ActionKeepAlive = act_modify:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(modify, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"move">>,  Req, State ) ->
    %% start gen_server for that action
    {ok, Pid} = act_move:start(State),

    %% check with keep alive
    ActionKeepAlive = act_move:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(move, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"notify">>,  Req, State ) ->
    %% start gen_server for that action
    {ok, Pid} = act_notify:start(State),

    %% check with keep alive
    ActionKeepAlive = act_notify:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(notify, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"pause">>,  Req, State ) ->
    %% start gen_server for that action
    {ok, Pid} = act_pause:start(State),

    %% check with keep alive
    ActionKeepAlive = act_pause:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(pause, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"query">>,  Req, State ) ->
    %% start gen_server for that action
    {ok, Pid} = act_query:start(State),

    %% check with keep alive
    ActionKeepAlive = act_query:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(query, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"redirect">>,  Req, State ) ->
    %% start gen_server for that action
    {ok, Pid} = act_redirect:start(State),

    %% check with keep alive
    ActionKeepAlive = act_redirect:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(redirect, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"remediate">>,  Req, State ) ->
    %% start gen_server for that action
    {ok, Pid} = act_remediate:start(State),

    %% check with keep alive
    ActionKeepAlive = act_remediate:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(remediate, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"report">>,  Req, State ) ->
    %% start gen_server for that action
    {ok, Pid} = act_report:start(State),

    %% check with keep alive
    ActionKeepAlive = act_report:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(report, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"restart">>,  Req, State ) ->
    %% start gen_server for that action
    {ok, Pid} = act_restart:start(State),

    %% check with keep alive
    ActionKeepAlive = act_restart:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(restart, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"restore">>,  Req, State ) ->
    %% start gen_server for that action
    {ok, Pid} = act_restore:start(State),

    %% check with keep alive
    ActionKeepAlive = act_restore:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(restore, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"resume">>,  Req, State ) ->
    %% start gen_server for that action
    {ok, Pid} = act_resume:start(State),

    %% check with keep alive
    ActionKeepAlive = act_resume:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(resume, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"save">>,  Req, State ) ->
    %% start gen_server for that action
    {ok, Pid} = act_save:start(State),

    %% check with keep alive
    ActionKeepAlive = act_save:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(save, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"scan">>,  Req, State ) ->
    %% start gen_server for that action
    {ok, Pid} = act_scan:start(State),

    %% check with keep alive
    ActionKeepAlive = act_scan:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(scan, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"set">>,  Req, State ) ->
    %% start gen_server for that action
    {ok, Pid} = act_set:start(State),

    %% check with keep alive
    ActionKeepAlive = act_set:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(set, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"snapshot">>,  Req, State ) ->
    %% start gen_server for that action
    {ok, Pid} = act_snapshot:start(State),

    %% check with keep alive
    ActionKeepAlive = act_snapshot:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(snapshot, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"start">>,  Req, State ) ->
    %% start gen_server for that action
    {ok, Pid} = act_start:start(State),

    %% check with keep alive
    ActionKeepAlive = act_start:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(start, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"stop">>,  Req, State ) ->
    %% start gen_server for that action
    {ok, Pid} = act_stop:start(State),

    %% check with keep alive
    ActionKeepAlive = act_stop:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(stop, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"substitute">>,  Req, State ) ->
    %% start gen_server for that action
    {ok, Pid} = act_substitute:start(State),

    %% check with keep alive
    ActionKeepAlive = act_substitute:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(substitute, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"sync">>,  Req, State ) ->
    %% start gen_server for that action
    {ok, Pid} = act_sync:start(State),

    %% check with keep alive
    ActionKeepAlive = act_sync:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(sync, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"throttle">>,  Req, State ) ->
    %% start gen_server for that action
    {ok, Pid} = act_throttle:start(State),

    %% check with keep alive
    ActionKeepAlive = act_throttle:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(throttle, Pid, ActionKeepAlive, Req, State);

spawn_action( <<"update">>,  Req, State ) ->
    %% see if server already started
    Started = whereis(act_update),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, Pid} = act_update:start(State);
        Started when is_pid(Started) ->
            %% already started
            Pid = Started
    end,

    %% check with keep alive
    ActionKeepAlive = act_update:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(update, Pid, ActionKeepAlive, Req, State);

spawn_action( _ActionSvr,  Req, State ) ->
    %% no function for this action so reply accordingly
    {ok, Req2} = cowboy_req:reply( 400
                                 , []
                                 , <<"Missing action function">>
                                 , Req
                                 ),
    {ok, Req2, State}.

action_valid(Action, Pid, ActionKeepAlive, Req, State) ->
    %% action was valid so update State
    State2 = maps:put(action_valid, true, State),
    State3 = maps:put(action, Action, State2),
    State4 = tools:add_pid(Action, Pid, State3),

    %% tail end recurse to verifying keepalive
    verify_keepalive( ActionKeepAlive, Req, State4).

verify_keepalive( {keepalive_received, Server}
                , Req
                , State
                ) ->
    %% keepalive worked as expected
    State2 = maps:put(action_keepalive, true, State),
    State3 = maps:put(action_server, Server, State2),

    %% recall whether has target and tail recurse
    HasTarget = maps:get(has_target, State3),
    lager:debug("HasTarget: ~p", [HasTarget]),
    %% tail recurse to handling target
    targets:get_target(HasTarget, Req, State3 );

verify_keepalive( UnexpectedKeepalive
                , Req
                , State
                ) ->
    %% didnot get expected keepalive response
    State2 = maps:put(action_keepalive, false, State),
    lager:info("~p UnexpectedKeepalive", [UnexpectedKeepalive]),
    {ok, Req2} = cowboy_req:reply( 400
                                 , []
                                 , <<"keepalive failed">>
                                 , Req
                                 ),
    %% don't continue on, return because of unexpected response
    {ok, Req2, State2}.

