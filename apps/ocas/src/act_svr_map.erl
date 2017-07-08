%%% @author Duncan Sparrell
%%% @copyright (C) 2017, sFractal Consulting LLC
%%%
-module(act_svr_map).
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

-export([ text_to_server/1      % given text return name of server
        , is_server_running/1   % given atom, is there a named server running
        , action_map/0          % return action map
        ]
       ).

%% return map of action binary text and atoms
-spec action_map( ) -> map().
action_map() ->
    ActionMap = #{ <<"allow">> => act_allow
                 , <<"augment">> => act_augment
                 , <<"cancel">> => act_cancel
                 , <<"contain">> => act_contain
                 , <<"copy">> => act_copy
                 , <<"delay">> => act_delay
                 , <<"delete">> => act_delete
                 , <<"deny">> => act_deny
                 , <<"detonate">> => act_detonate
                 , <<"distill">> => act_distill
                 , <<"investigate">> => act_investigate
                 , <<"locate">> => act_locate
                 , <<"mitigate">> => act_mitigate
                 , <<"move">> => act_move
                 , <<"notify">> => act_notify
                 , <<"pause">> => act_pause
                 , <<"query">> => act_query
                 , <<"redirect">> => act_redirect
                 , <<"remediate">> => act_remediate
                 , <<"report">> => act_report
                 , <<"restart">> => act_restart
                 , <<"restore">> => act_restore
                 , <<"resume">> => act_resume
                 , <<"save">> => act_save
                 , <<"scan">> => act_scan
                 , <<"set">> => act_set
                 , <<"snapshot">> => act_snapshot
                 , <<"start">> => act_start
                 , <<"stop">> => act_stop
                 , <<"substitute">> => act_substitute
                 , <<"sync">> => act_sync
                 , <<"throttle">> => act_throttle
                 , <<"update">> => act_update
                 },
    ActionMap.

-spec text_to_server( bitstring() ) -> atom().
text_to_server( Word ) ->
    lager:info("text_to_server ~p", [Word]),
    ActionMap = action_map(),
    %% return value for key=Word
    Svr = maps:get(Word, ActionMap, bad_action),
    lager:info("text_to_server:Svr ~p", [Svr]),
    Svr.

%% return true if server already running
-spec is_server_running(atom()) -> 'undefined' | pid() | port().
is_server_running(Svr) ->
    whereis(Svr).

