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
-compile(export_all).

%% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%% remove this once working
-include_lib("./include/allow01.hrl").

%% tests to run
all() ->
    [ test_allow
    , test_allow_again
    , test_mitigate
    , test_mitigate_again
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
    helper_json:post_oc2("allow01.json"
                        , "allow01.results.json"
                        , Config
                        ),
    ok.

test_allow_again(Config) ->
    %% run again to exercise different legs of code when servers already running
    helper_json:post_oc2("allow01.json"
                        , "allow01.results.json"
                        , Config
                        ),


    ok.

test_mitigate(Config) ->

    %% send command and compare expected results
    helper_json:post_oc2("mitigate01.json"
                        , "mitigate01.results.json"
                        , Config
                        ),
    ok.

test_mitigate_again(Config) ->
    %% run again to exercise different legs of code when servers already running

    %% send command and compare expected results
    helper_json:post_oc2("mitigate01.json"
                        , "mitigate01.results.json"
                        , Config
                        ),
    ok.

