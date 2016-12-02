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
%% @doc test pause action
%% @end
%%%-------------------------------------------------------------------

-module(pause_SUITE).
-author("Duncan Sparrell").
-copyright("2016, sFractal Consulting, LLC").
-license(apache2).

%% for test export all functions
-compile(export_all).

%% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%% includes of common test json data
-include_lib("./include/pause01.hrl").

%% tests to run
all() ->
    [ test_pause
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

test_pause(_Config) ->

    ReqHeaders = [ {<<"content-type">>
                 , <<"application/json">>}
                 ],

    Url = "/openc2",

    Options = #{},

    Json = ?PAUSE01,

    %% validate the json
    true = jsx:is_json(Json),

    %% send the json in the body of the request
    ReqBody = Json,

    %% expect to get 200 status code
    ExpectedStatus = 200,

    %% for now command to reply with dummy response (json of State)
    %% decode the json and check for key/values of interest
    ExpectedJsonPairs = [ {<<"has_http_body">>, true}
                        , {<<"good_json">>, true}
                        , {<<"has_action">>, true}
                        , {<<"action">>, <<"pause">>}
                        , {<<"action_server">>, <<"pause_server">>}
                        , {<<"action_valid">>, true}
                        , {<<"has_actuator">>, true}
                        , {<<"has_modifiers">>, true}
                        , {<<"has_target">>, true}
                        , {<<"action_keepalive">>, true}
                        ],

    %% send request, test response
    helper:send_recieve( ReqHeaders       % to send
                , Options          % to send
                , ReqBody          % to send
                , Url              % to send
                , ExpectedStatus  % test get this received
                , ExpectedJsonPairs
                ),

    ok.
