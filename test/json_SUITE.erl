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

end_per_suite(Config) ->
    Config.

test_allow(Config) ->

    %% retrieve the json filename
    JsonFile = filename:join( ?config( data_dir, Config ), "allow01.json" ),
    lager:debug("testdata: ~p", [JsonFile]),

    %% retrieve the text in json file
    {ok, Json} = file:read_file(JsonFile),

    %% validate the json
    true = jsx:is_json(Json),

    %% retrieve the expected results (in json)
    JsonExpFile = filename:join( ?config( data_dir, Config )
                               , "allow01.results.json" ),

    %% retrieve the text in json file
    {ok, JsonResultsTxt} = file:read_file(JsonExpFile),

    true = jsx:is_json(JsonResultsTxt),

    %% convert to erlang terms
    JsonResults = jsx:decode(JsonResultsTxt, [return_maps]),

%%%%%%%%%%%%%%%%

    ReqHeaders = [ {<<"content-type">>
                 , <<"application/json">>}
                 ],

    Url = "/openc2",

    Options = #{},

    %% send the json in the body of the request
    ReqBody = Json,

    %% expect to get the ExpectedStatus status code
    ExpectedStatus = maps:get(<<"ExpectedStatus">>, JsonResults),

    %% for now command to reply with dummy response (json of State)
    %% decode the json and check for key/values of interest
    ExpectedJsonPairs = [ {<<"has_http_body">>, true}
                        , {<<"good_json">>, true}
                        , {<<"has_action">>, true}
                        , {<<"action">>, <<"allow">>}
                        , {<<"action_server">>, <<"act_allow">>}
                        , {<<"action_valid">>, true}
                        , {<<"has_actuator">>, true}
                        , {<<"has_modifiers">>, true}
                        , {<<"has_target">>, true}
                        , {<<"action_keepalive">>, true}
                        ],
%
    ExpectedJsonPairs = maps:get(<<"ExpectedJsonPairs">>, JsonResults),

    %% send request, test response
    helper:send_recieve( ReqHeaders       % to send
                , Options          % to send
                , ReqBody          % to send
                , Url              % to send
                , ExpectedStatus  % test get this received
                , ExpectedJsonPairs
                ),

    ok.

test_allow_again(_Config) ->

    ReqHeaders = [ {<<"content-type">>
                 , <<"application/json">>}
                 ],

    Url = "/openc2",

    Options = #{},

    Json = ?ALLOW01,

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
                        , {<<"action">>, <<"allow">>}
                        , {<<"action_server">>, <<"act_allow">>}
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

