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

-module(action_ir_SUITE).
-author("Duncan Sparrell").
-copyright("2017, sFractal Consulting, LLC").
-license(apache2).

%% for test export all functions
-export([ all/0
        , suite/0
        , init_per_suite/1
        , end_per_suite/1
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
        ]).

%% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%% tests to run
all() ->
    [ test_investigate
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

