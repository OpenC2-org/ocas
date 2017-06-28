%%% @author Duncan Sparrell
%%% @copyright (C) 2017, sFractal Consulting LLC
%%%
-module(oc_svr_stop).

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

-export([ stop/1
        , todo/0
        ]
       ).

%% delete this function - it's a placeholder on work in progress
%%     to keep xref happy
-ignore_xref({todo, 0}).
todo() ->
    A = act_svr_map:action_map(),
    B = act_svr_map:is_server_running( junk ),
    C = act_svr_map:text_to_server( <<"junk">> ),
    D = oc_env:keepalive(),
    E = oc_env:stop(),
    F = oc_env:terminate(),
    G = tgt_net_con:keepalive(),
    H = tgt_net_con:start( #{} ),

    {A, B, C, D, E, F, G, H}.

stop( [] ) ->
    %% empty list so done
    ok;

%% stop first server on list and recurse
stop( [ act_allow |  NewSvrList ] ) ->
    act_allow:stop(),
    stop(NewSvrList);

stop( [ act_augment |  NewSvrList ] ) ->
    act_augment:stop(),
    stop(NewSvrList);

stop( [ act_cancel |  NewSvrList ] ) ->
    act_cancel:stop(),
    stop(NewSvrList);

stop( [ act_contain |  NewSvrList ] ) ->
    act_contain:stop(),
    stop(NewSvrList);

stop( [ act_copy |  NewSvrList ] ) ->
    act_copy:stop(),
    stop(NewSvrList);

stop( [ act_delay |  NewSvrList ] ) ->
    act_delay:stop(),
    stop(NewSvrList);

stop( [ act_deny |  NewSvrList ] ) ->
    act_deny:stop(),
    stop(NewSvrList);

stop( [ act_delete |  NewSvrList ] ) ->
    act_delete:stop(),
    stop(NewSvrList);

stop( [ act_detonate |  NewSvrList ] ) ->
    act_detonate:stop(),
    stop(NewSvrList);

stop( [ act_distill |  NewSvrList ] ) ->
    act_distill:stop(),
    stop(NewSvrList);

stop( [ act_investigate |  NewSvrList ] ) ->
    act_investigate:stop(),
    stop(NewSvrList);

stop( [ act_locate |  NewSvrList ] ) ->
    act_locate:stop(),
    stop(NewSvrList);

stop( [ act_mitigate |  NewSvrList ] ) ->
    act_mitigate:stop(),
    stop(NewSvrList);

stop( [ act_modify |  NewSvrList ] ) ->
    act_modify:stop(),
    stop(NewSvrList);

stop( [ act_move |  NewSvrList ] ) ->
    act_move:stop(),
    stop(NewSvrList);

stop( [ act_notify |  NewSvrList ] ) ->
    act_notify:stop(),
    stop(NewSvrList);

stop( [ act_pause |  NewSvrList ] ) ->
    act_pause:stop(),
    stop(NewSvrList);

stop( [ act_query |  NewSvrList ] ) ->
    act_query:stop(),
    stop(NewSvrList);

stop( [ act_redirect |  NewSvrList ] ) ->
    act_redirect:stop(),
    stop(NewSvrList);

stop( [ act_remediate |  NewSvrList ] ) ->
    act_remediate:stop(),
    stop(NewSvrList);

stop( [ act_report |  NewSvrList ] ) ->
    act_report:stop(),
    stop(NewSvrList);

stop( [ act_restart |  NewSvrList ] ) ->
    act_restart:stop(),
    stop(NewSvrList);

stop( [ act_restore |  NewSvrList ] ) ->
    act_restore:stop(),
    stop(NewSvrList);

stop( [ act_resume |  NewSvrList ] ) ->
    act_resume:stop(),
    stop(NewSvrList);

stop( [ act_save | NewSvrList ] ) ->
    act_save:stop(),
    stop(NewSvrList);

stop( [ act_scan |  NewSvrList ] ) ->
    act_scan:stop(),
    stop(NewSvrList);

stop( [ act_set |  NewSvrList ] ) ->
    act_set:stop(),
    stop(NewSvrList);

stop( [ act_snapshot |  NewSvrList ] ) ->
    act_snapshot:stop(),
    stop(NewSvrList);

stop( [ act_start |  NewSvrList ] ) ->
    act_start:stop(),
    stop(NewSvrList);

stop( [ act_stop |  NewSvrList ] ) ->
    act_stop:stop(),
    stop(NewSvrList);

stop( [ act_substitute |  NewSvrList ] ) ->
    act_substitute:stop(),
    stop(NewSvrList);

stop( [ act_sync |  NewSvrList ] ) ->
    act_sync:stop(),
    stop(NewSvrList);

stop( [ act_throttle |  NewSvrList ] ) ->
    act_throttle:stop(),
    stop(NewSvrList);

stop( [ act_update |  NewSvrList ] ) ->
    act_update:stop(),
    stop(NewSvrList);

stop( [ acu_demense |  NewSvrList ] ) ->
    acu_demense:stop(),
    stop(NewSvrList);

stop( [ acu_network_firewall |  NewSvrList ] ) ->
    acu_network_firewall:stop(),
    stop(NewSvrList);

stop( [ acu_network_router |  NewSvrList ] ) ->
    acu_network_router:stop(),
    stop(NewSvrList);

stop( [ acu_network_scanner |  NewSvrList ] ) ->
    acu_network_scanner:stop(),
    stop(NewSvrList);

stop( [ mod_ack |  NewSvrList ] ) ->
    mod_ack:stop(),
    stop(NewSvrList);

stop( [ mod_date_time |  NewSvrList ] ) ->
    mod_date_time:stop(),
    stop(NewSvrList);

stop( [ mod_delay |  NewSvrList ] ) ->
    mod_delay:stop(),
    stop(NewSvrList);

stop( [ mod_duration |  NewSvrList ] ) ->
    mod_duration:stop(),
    stop(NewSvrList);

stop( [ mod_id |  NewSvrList ] ) ->
    mod_id:stop(),
    stop(NewSvrList);

stop( [ mod_where |  NewSvrList ] ) ->
    mod_where:stop(),
    stop(NewSvrList);

stop( [ tgt_hostname |  NewSvrList ] ) ->
    tgt_hostname:stop(),
    stop(NewSvrList);

stop( [ tgt_ipv4_address |  NewSvrList ] ) ->
    tgt_ipv4_address:stop(),
    stop(NewSvrList);

stop( [ tgt_ipv6_address |  NewSvrList ] ) ->
    tgt_ipv6_address:stop(),
    stop(NewSvrList);

stop( [ tgt_net_con |  NewSvrList ] ) ->
    tgt_net_con:stop(),
    stop(NewSvrList);

stop( [ tgt_network_firewall |  NewSvrList ] ) ->
    tgt_network_firewall:stop(),
    stop(NewSvrList);

stop( [ tgt_network_scanner |  NewSvrList ] ) ->
    tgt_network_scanner:stop(),
    stop(NewSvrList);

stop( [ Svr |  NewSvrList ] ) ->
    %% stop first server on list and recurse
    lager:error("lambda ok on svr stop? should it be softer and recursive"),
    lager:error("or should this be supervison tree"),
    die = Svr,
    stop(NewSvrList).

