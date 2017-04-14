%%% @author Duncan Sparrell
%%% @copyright (C) 2017, sFractal Consulting LLC
%%%
-module(actions).
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
            {ok, _Pid} = act_allow:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_allow:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(allow, ActionKeepAlive, Req, State);

spawn_action( <<"augment">>,  Req, State ) ->
    %% see if server already started
    Started = whereis(act_augment),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_augment:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_augment:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(augment, ActionKeepAlive, Req, State);

spawn_action( <<"cancel">>,  Req, State ) ->
    %% see if server already started
    Started = whereis(act_cancel),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_cancel:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,
    %% check with keep alive
    ActionKeepAlive = act_cancel:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(cancel, ActionKeepAlive, Req, State);

spawn_action( <<"contain">>,  Req, State ) ->

    %% see if server already started
    Started = whereis(act_contain),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_contain:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_contain:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(contain, ActionKeepAlive, Req, State);

spawn_action( <<"copy">>,  Req, State ) ->

    %% see if server already started
    Started = whereis(act_copy),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_copy:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_copy:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(copy, ActionKeepAlive, Req, State);

spawn_action( <<"delay">>,  Req, State ) ->

    %% see if server already started
    Started = whereis(act_delay),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_delay:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_delay:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(delay, ActionKeepAlive, Req, State);

spawn_action( <<"delete">>,  Req, State ) ->
    %% see if server already started
    Started = whereis(act_delete),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_delete:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_delete:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(delete, ActionKeepAlive, Req, State);

spawn_action( <<"deny">>,  Req, State ) ->
    %% see if server already started
    Started = whereis(act_deny),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_deny:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_deny:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(deny, ActionKeepAlive, Req, State);

spawn_action( <<"detonate">>,  Req, State ) ->

    %% see if server already started
    Started = whereis(act_detonate),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_detonate:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_detonate:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(detonate, ActionKeepAlive, Req, State);

spawn_action( <<"distill">>,  Req, State ) ->

    %% see if server already started
    Started = whereis(act_distill),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_distill:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_distill:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(distill, ActionKeepAlive, Req, State);

spawn_action( <<"get">>,  Req, State ) ->

    %% see if server already started
    Started = whereis(act_get),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_get:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_get:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(get, ActionKeepAlive, Req, State);

spawn_action( <<"investigate">>,  Req, State ) ->

    %% see if server already started
    Started = whereis(act_investigate),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_investigate:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_investigate:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(investigate, ActionKeepAlive, Req, State);

spawn_action( <<"locate">>,  Req, State ) ->

    %% see if server already started
    Started = whereis(act_locate),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_locate:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_locate:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(locate, ActionKeepAlive, Req, State);

spawn_action( <<"mitigate">>,  Req, State ) ->
    %% see if server already started
    Started = whereis(act_mitigate),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_mitigate:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_mitigate:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(mitigate, ActionKeepAlive, Req, State);

spawn_action( <<"modify">>,  Req, State ) ->

    %% see if server already started
    Started = whereis(act_modify),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_modify:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_modify:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(modify, ActionKeepAlive, Req, State);

spawn_action( <<"move">>,  Req, State ) ->

    %% see if server already started
    Started = whereis(act_move),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_move:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_move:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(move, ActionKeepAlive, Req, State);

spawn_action( <<"notify">>,  Req, State ) ->

    %% see if server already started
    Started = whereis(act_notify),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_notify:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_notify:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(notify, ActionKeepAlive, Req, State);

spawn_action( <<"pause">>,  Req, State ) ->

    %% see if server already started
    Started = whereis(act_pause),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_pause:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_pause:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(pause, ActionKeepAlive, Req, State);

spawn_action( <<"query">>,  Req, State ) ->

    %% see if server already started
    Started = whereis(act_query),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_query:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_query:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(query, ActionKeepAlive, Req, State);

spawn_action( <<"redirect">>,  Req, State ) ->

    %% see if server already started
    Started = whereis(act_redirect),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_redirect:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_redirect:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(redirect, ActionKeepAlive, Req, State);

spawn_action( <<"remediate">>,  Req, State ) ->

    %% see if server already started
    Started = whereis(act_remediate),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_remediate:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_remediate:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(remediate, ActionKeepAlive, Req, State);

spawn_action( <<"report">>,  Req, State ) ->

    %% see if server already started
    Started = whereis(act_report),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_report:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_report:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(report, ActionKeepAlive, Req, State);

spawn_action( <<"restart">>,  Req, State ) ->

    %% see if server already started
    Started = whereis(act_restart),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_restart:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_restart:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(restart, ActionKeepAlive, Req, State);

spawn_action( <<"restore">>,  Req, State ) ->

    %% see if server already started
    Started = whereis(act_restore),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_restore:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_restore:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(restore, ActionKeepAlive, Req, State);

spawn_action( <<"resume">>,  Req, State ) ->

    %% see if server already started
    Started = whereis(act_resume),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_resume:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_resume:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(resume, ActionKeepAlive, Req, State);

spawn_action( <<"save">>,  Req, State ) ->

    %% see if server already started
    Started = whereis(act_save),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_save:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_save:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(save, ActionKeepAlive, Req, State);

spawn_action( <<"scan">>,  Req, State ) ->

    %% see if server already started
    Started = whereis(act_scan),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_scan:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_scan:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(scan, ActionKeepAlive, Req, State);

spawn_action( <<"set">>,  Req, State ) ->

    %% see if server already started
    Started = whereis(act_set),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_set:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_set:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(set, ActionKeepAlive, Req, State);

spawn_action( <<"snapshot">>,  Req, State ) ->

    %% see if server already started
    Started = whereis(act_snapshot),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_snapshot:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_snapshot:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(snapshot, ActionKeepAlive, Req, State);

spawn_action( <<"start">>,  Req, State ) ->

    %% see if server already started
    Started = whereis(act_start),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_start:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_start:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(start, ActionKeepAlive, Req, State);

spawn_action( <<"stop">>,  Req, State ) ->

    %% see if server already started
    Started = whereis(act_stop),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_stop:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_stop:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(stop, ActionKeepAlive, Req, State);

spawn_action( <<"substitute">>,  Req, State ) ->

    %% see if server already started
    Started = whereis(act_substitute),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_substitute:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_substitute:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(substitute, ActionKeepAlive, Req, State);

spawn_action( <<"sync">>,  Req, State ) ->

    %% see if server already started
    Started = whereis(act_sync),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_sync:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_sync:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(sync, ActionKeepAlive, Req, State);

spawn_action( <<"throttle">>,  Req, State ) ->

    %% see if server already started
    Started = whereis(act_throttle),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_throttle:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_throttle:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(throttle, ActionKeepAlive, Req, State);

spawn_action( <<"update">>,  Req, State ) ->
    %% see if server already started
    Started = whereis(act_update),

    case Started of
        undefined ->
            %% spawn process since not started yet
            {ok, _Pid} = act_update:start(State);
        Started when is_pid(Started) ->
            %% already started
            ok
    end,

    %% check with keep alive
    ActionKeepAlive = act_update:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),

    %% tail end recurse
    action_valid(update, ActionKeepAlive, Req, State);

spawn_action( _ActionSvr,  Req, State ) ->
    %% no function for this action so reply accordingly
    {ok, Req2} = cowboy_req:reply( 400
                                 , []
                                 , <<"Missing action function">>
                                 , Req
                                 ),
    {ok, Req2, State}.

action_valid(Action, ActionKeepAlive, Req, State) ->
    %% action was valid so update State
    State2 = maps:put(action_valid, true, State),
    State3 = maps:put(action, Action, State2),

    %% tail end recurse to verifying keepalive
    verify_keepalive( ActionKeepAlive, Req, State3).

verify_keepalive( {keepalive_received, Server}
                , Req
                , State
                ) ->
    %% keepalive worked as expected
    State2 = maps:put(action_keepalive, true, State),
    State3 = maps:put(action_server, Server, State2),

    %% tell oc_env server that allow is now running
    ok = oc_env:server_up(Server),

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

