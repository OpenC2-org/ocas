%%%-------------------------------------------------------------------
%%% Copyright (c) 2016, sFractal Consulting, LLC

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
%% @doc test env server
%% @end
%%%-------------------------------------------------------------------

-module(env_SUITE).
-author("Duncan Sparrell").
-copyright("2017, sFractal Consulting, LLC").
-license(apache2).

%% for test export all functions
-compile(export_all).

%% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%% includes of common test json data
-include_lib("./include/env01.hrl").

%% tests to run
all() ->
    [ test_status0          %% test status prior to env being initialized
    , test_init_lang        %% test env being initialized for language
    , test_status1           %% test status after env initialized
    , test_init_again       %% test status again after env already running
                            %% add actuator and orchestrator later
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

    %% start application
    {ok, _AppList3} = application:ensure_all_started(ocas),

    Config.

test_init_lang(Config)  ->
    lager:info("test_init_lang Config ~p", [Config] ),
    %% initialize simulator to language

    ReqHeaders = [ {<<"content-type">>
                 , <<"application/json">>}
                 ],

    Url = "/init",

    Options = #{},

    Json = ?ENV01,    % json for init to language

    %% validate the json
    true = jsx:is_json(Json),

    %% send the json in the body of the request
    ReqBody = Json,

    %% expect to get 200 status code
    ExpectedStatus = 200,

    %% decode the json and check for key/values of interest
    ExpectedJsonPairs = [ {<<"simulator_type">>, <<"language">>}
                        , {<<"restart_count">>, 1}
                        , {<<"init_state">>, #{} }
                        ],
    %% look for keys in json, but not values
    ExpectedJsonKeys = [ <<"this_machine">>
                       , <<"svr_list">>
                       , <<"start_time">>
                       ],

    %% send request, test response
    helper:send_receive( post
                , Url              % to send
                , ReqHeaders       % to send
                , Options          % to send
                , ReqBody          % to send
                , ExpectedStatus  % test get this received
                , ExpectedJsonKeys % see if these keys in received json
                , ExpectedJsonPairs % check these pairs in received json
                ),

    ok.

test_status0(_Config)  ->
    %% test http GET status from initial setup
    test_status(0).

test_status1(_Config)  ->
    %% test http GET status after restart to language
    test_status(1).

test_status(RestartCount)  ->
    %% set up getting status via 'GET' (note env already running)

    Url = "/status",
    Headers = [ ],
    Options = #{},

    %% expect to get 200 status code
    ExpectedStatus = 200,

    %% decode the json and check for key/values of interest
    ExpectedJsonPairs = [ {<<"simulator_type">>, <<"language">>}
                        , {<<"restart_count">>, RestartCount}
                        , {<<"init_state">>, #{} }
                        ],
    %% look for keys in json, but not values
    ExpectedJsonKeys = [ <<"this_machine">>
                       , <<"svr_list">>
                       , <<"start_time">>
                       ],

    %% send request, test response
    helper:send_receive( get
                       , Url              % to send
                       , Headers       % to send
                       , Options          % to send
                       , ExpectedStatus  % test get this received
                       , ExpectedJsonKeys
                       , ExpectedJsonPairs
                       ),

    ok.

test_init_again(_Config)  ->
    % env already running and re-initialize to language

    ReqHeaders = [ {<<"content-type">>
                 , <<"application/json">>}
                 ],

    Url = "/init",

    Options = #{},

    Json = ?ENV01,    % json for init to language

    %% validate the json
    true = jsx:is_json(Json),

    %% send the json in the body of the request
    ReqBody = Json,

    %% expect to get 200 status code
    ExpectedStatus = 200,

    %% decode the json and check for key/values of interest
    ExpectedJsonPairs = [ {<<"simulator_type">>, <<"language">>}
                        , {<<"restart_count">>, 2}
                        , {<<"init_state">>, #{} }
                        ],
    %% look for keys in json, but not values
    ExpectedJsonKeys = [ <<"this_machine">>
                       , <<"svr_list">>
                       , <<"start_time">>
                       ],

    %% send request, test response
    helper:send_receive( post
                , Url              % to send
                , ReqHeaders       % to send
                , Options          % to send
                , ReqBody          % to send
                , ExpectedStatus  % test get this received
                , ExpectedJsonKeys % see if these keys in received json
                , ExpectedJsonPairs % check these pairs in received json
                ),

    ok.
