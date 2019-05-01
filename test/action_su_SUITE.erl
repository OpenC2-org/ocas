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

-module(action_su_SUITE).
-author("Duncan Sparrell").
-copyright("2017, sFractal Consulting, LLC").
-license(apache2).

%% for test export all functions
-export([ all/0
        , suite/0
        , init_per_suite/1
        , end_per_suite/1
        , test_save/1
        , test_scan/1
        , test_set/1
        , test_snapshot/1
        , test_start/1
        , test_stop/1
        , test_substitute/1
        , test_sync/1
        , test_update/1
        ]).

%% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%% tests to run
all() ->
    [ test_save
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

test_update(Config) ->
    %% send command and compare expected results
    helper_json:post_oc2( "update01.json"
                        , "update01.results.json"
                        , Config
                        ),
    ok.
