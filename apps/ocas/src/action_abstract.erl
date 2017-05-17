%%% @author Duncan Sparrell
%%% @copyright (C) 2017, sFractal Consulting LLC
%%%
-module(action_abstract).
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

-export([ get_action/3 ]).

%% software design note:
%%      this is trying out behaviour and this is the abstract module

%% Sunny Day flow
%%     get_action               ie entry module
%%  |> check_action             ie is action on list of valid actions
%%  |> action_running           ie see if up already, if not then start
%%  |> action_valid             ie send keepalive
%%  |> verify_keepalive         ie validate server up
%%  |> targets:get_target       ie tail recurse on 

get_action( BinAction,  Req, State ) ->
    lager:info("get_action ~p", [BinAction]),
    %% lookup action server for this word
    Action = act_svr_map:text_to_server(BinAction),
    lager:info("get_action ~p", [Action]),
    %% svr if valid, bad_action if not

    %% check Action was valid
    check_action( Action,  Req, State ).

check_action( bad_action,  Req, State ) ->
    lager:info("bad_action"),
    %% bad action
    State2 = maps:put(bad_action, true, State),
    lager:info("bad_action2"),
    {ok, Req2} = cowboy_req:reply( 400
                                 , []
                                 , <<"bad action">>
                                 , Req
                                 ),
    lager:info("bad_action3"),
    %% don't continue on, return because of unexpected response
    %%    halt since now done with the request
    {halt, Req2, State2};

check_action( Action,  Req, State ) ->
    %% since wasn't bad_action then must be valid action

    %% see if already running
    %%        undefined if not running,
    %%        is_pid() if running
    ActionPid = act_svr_map:is_server_running(Action),
    lager:info("action_abstract:check_action:ActionPid ~p", [ActionPid]),

    %% tail recurse on
    action_running( ActionPid, Action,  Req, State ).

action_running( undefined, Action,  Req, State ) ->
    %% not started yet so start
    lager:info("action_running, not started, Action=~p", [Action]),
    ok = oc_env:start_server(Action, State),
    lager:info("todo:action_abstract:action_running update state"),
NewState = State,  %fix this
     
    %% tail recurse on
    action_valid(Action, Req, NewState);


action_running( ActionPid, Action,  Req, State )
        when is_pid(ActionPid) ->
    %% server already running
% do we need to tell env about this command
lager:debug("do we need to tell env about this command"),
% oc_env:update_server({Action, State}),

    %% tail recurse on
    action_valid(Action, Req, State);
    

action_running( _ActionPid, Action,  Req, State ) ->
    %% got wonky answer so abort
    State2 = maps:put(action_keepalive, false, State),
    lager:info("~p server issue", [Action]),
    {ok, Req2} = cowboy_req:reply( 400
                                 , []
                                 , <<"Action Server Issue">>
                                 , Req
                                 ),
    %% don't continue on, return because of unexpected response
    {ok, Req2, State2}.

%% valid action so validate it and move on
action_valid(Action, Req, State) ->
    %% send a keepalive to validate server up
%still needed???
lager:debug("is keepalive needed here"),
%maybe replace with keepalive in env
lager:debug("maybe replace with keepalive in env"),
    ActionKeepAlive = Action:keepalive(),
    lager:debug("ActionKeepAlive: ~p ", [ActionKeepAlive]),
    %% check if valid keepalive received
    action_valid(Action, ActionKeepAlive, Req, State).

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

