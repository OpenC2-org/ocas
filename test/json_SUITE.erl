%%%-------------------------------------------------------------------
%%% Copyright (c) 2017, sFractal Consulting, LLC

%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at

%%%     http://www.apache.org/licenses/LICENSE-2.0

%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.

%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% @doc test a series of openc2 json commands
%% @end
%%%-------------------------------------------------------------------

-module(json_SUITE).
-author("Duncan Sparrell").
-copyright("2017, sFractal Consulting, LLC").
-license(apache2).

%% for test export all functions
%-compile(export_all).
-export( [ all/0
         , suite/0
         , init_per_suite/1
         , end_per_suite/1
         , test_allow/1
         , test_augment/1
         , test_cancel/1
         , test_contain/1
         , test_copy/1
         , test_delay/1
         , test_delete/1
         , test_demense/1
         , test_deny/1
         , test_detonate/1
         , test_distill/1
         , test_investigate/1
         , test_locate/1
         , test_mitigate/1
         , test_move/1
         , test_notify/1
         , test_pause/1
         , test_query/1
         , test_remediate/1
         , test_redirect/1
         , test_report/1
         , test_restart/1
         , test_restore/1
         , test_resume/1
         , test_save/1
         , test_scan/1
         , test_set/1
         , test_snapshot/1
         , test_start/1
         , test_stop/1
         , test_substitute/1
         , test_sync/1
         , test_throttle/1
         , test_update/1
         ] ).

%% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%% tests to run
all() ->
    [ test_allow
    , test_allow
    , test_augment
    , test_augment
    , test_cancel
    , test_cancel
    , test_contain
    , test_contain
    , test_copy
    , test_copy
    , test_delay
    , test_delay
    , test_delete
    , test_delete
    , test_demense
    , test_demense
    , test_deny
    , test_deny
    , test_detonate
    , test_detonate
    , test_distill
    , test_distill
    , test_investigate
    , test_investigate
    , test_locate
    , test_locate
    , test_mitigate
    , test_mitigate
    , test_move
    , test_move
    , test_notify
    , test_notify
    , test_pause
    , test_pause
    , test_query
    , test_query
    , test_remediate
    , test_remediate
    , test_redirect
    , test_redirect
    , test_report
    , test_report
    , test_restart
    , test_restart
    , test_restore
    , test_restore
    , test_resume
    , test_resume
    , test_save
    , test_save
    , test_scan
    , test_scan
    , test_set
    , test_set
    , test_snapshot
    , test_snapshot
    , test_start
    , test_start
    , test_stop
    , test_stop
    , test_substitute
    , test_substitute
    , test_sync
    , test_sync
    , test_throttle
    , test_throttle
    , test_update
    , test_update
    ].

%% timeout if no reply in a minute
suite() ->
    [{timetrap, {minutes, 2}}].

%% setup config parameters
init_per_suite(Config) ->
    {ok, _AppList} = application:ensure_all_started(lager),

    {ok, _AppList2} = application:ensure_all_started(shotgun),

    %% since ct doesn't read sys.config, set configs here
    application:set_env(ocas, port, 8080),
    application:set_env(ocas, listener_count, 5),

    %% set up logging
    application:set_env(lager
                       , handlers
                       , [ {lager_console_backend, debug}
                         , {lager_common_test_backend, debug}
                         ]
                       ),
    lager:set_loglevel(lager_console_backend, debug),

    %% start application
    {ok, _AppList3} = application:ensure_all_started(ocas),

    Config.

end_per_suite(Config) ->
    Config.

test_allow(Config) ->

    %% send allow01.json and get allow01.results.json
    helper_json:post_oc2( "allow01.json"
                        , "allow01.results.json"
                        , Config
                        ),
    ok.

test_augment(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "augment01.json"
                        , "augment01.results.json"
                        , Config
                        ),
    ok.

test_cancel(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "cancel01.json"
                        , "cancel01.results.json"
                        , Config
                        ),
    ok.

test_contain(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "contain01.json"
                        , "contain01.results.json"
                        , Config
                        ),
    ok.

test_copy(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "copy01.json"
                        , "copy01.results.json"
                        , Config
                        ),
    ok.

test_delay(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "delay01.json"
                        , "delay01.results.json"
                        , Config
                        ),
    ok.

test_delete(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "delete01.json"
                        , "delete01.results.json"
                        , Config
                        ),
    ok.

test_demense(Config) ->

    %% send command and compare expected results
    helper_json:post_oc2( "mitigate01.json"
                        , "mitigate01.results.json"
                        , Config
                        ),
    ok.


test_deny(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "deny01.json"
                        , "deny01.results.json"
                        , Config
                        ),
    ok.

test_detonate(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "detonate01.json"
                        , "detonate01.results.json"
                        , Config
                        ),
    ok.

test_distill(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "distill01.json"
                        , "distill01.results.json"
                        , Config
                        ),
    ok.

test_investigate(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "investigate01.json"
                        , "investigate01.results.json"
                        , Config
                        ),
    ok.

test_locate(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "locate01.json"
                        , "locate01.results.json"
                        , Config
                        ),
    ok.

test_mitigate(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "mitigate01.json"
                        , "mitigate01.results.json"
                        , Config
                        ),
    ok.


test_move(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "move01.json"
                        , "move01.results.json"
                        , Config
                        ),
    ok.

test_notify(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "notify01.json"
                        , "notify01.results.json"
                        , Config
                        ),
    ok.

test_pause(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "pause01.json"
                        , "pause01.results.json"
                        , Config
                        ),
    ok.

test_query(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "query01.json"
                        , "query01.results.json"
                        , Config
                        ),
    ok.


test_remediate(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "remediate01.json"
                        , "remediate01.results.json"
                        , Config
                        ),
    ok.

test_redirect(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "redirect01.json"
                        , "redirect01.results.json"
                        , Config
                        ),
    ok.

test_report(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "report01.json"
                        , "report01.results.json"
                        , Config
                        ),
    ok.

test_restart(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "restart01.json"
                        , "restart01.results.json"
                        , Config
                        ),
    ok.

test_restore(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "restore01.json"
                        , "restore01.results.json"
                        , Config
                        ),
    ok.

test_resume(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "resume01.json"
                        , "resume01.results.json"
                        , Config
                        ),
    ok.

test_save(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "save01.json"
                        , "save01.results.json"
                        , Config
                        ),
    ok.

test_scan(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "scan01.json"
                        , "scan01.results.json"
                        , Config
                        ),
    ok.

test_set(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "set01.json"
                        , "set01.results.json"
                        , Config
                        ),
    ok.

test_snapshot(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "snapshot01.json"
                        , "snapshot01.results.json"
                        , Config
                        ),
    ok.

test_start(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "start01.json"
                        , "start01.results.json"
                        , Config
                        ),
    ok.

test_stop(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "stop01.json"
                        , "stop01.results.json"
                        , Config
                        ),
    ok.

test_substitute(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "substitute01.json"
                        , "substitute01.results.json"
                        , Config
                        ),
    ok.

test_sync(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "sync01.json"
                        , "sync01.results.json"
                        , Config
                        ),
    ok.

test_throttle(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "throttle01.json"
                        , "throttle01.results.json"
                        , Config
                        ),
    ok.

test_update(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "update01.json"
                        , "update01.results.json"
                        , Config
                        ),
    ok.

