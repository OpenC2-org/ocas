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

-module(action_ad_SUITE).
-author("Duncan Sparrell").
-copyright("2017, sFractal Consulting, LLC").
-license(apache2).

%% for test export all functions
-export([ all/0
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
        ]).

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
    helper_json:post_oc2( "demense01.json"
                        , "demense01.results.json"
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

