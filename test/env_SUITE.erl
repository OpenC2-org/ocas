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
    [ test_no_init          %% test status prior to env being initialized
    , test_init_lang        %% test env being initialized for language
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

test_no_init(Config) ->
    lager:info("test_no_init Config ~p", [Config] ),
    %% set up getting status via 'GET' (note env not initialized yet)
    MyPort = application:get_env(ocas, port, 8080),
    {ok, Conn} = shotgun:open("localhost", MyPort),
    Headers = [ {<<"content-type">>, <<"application/text">>} ],
    Options = #{},
    ResponseToGet = shotgun:get(Conn, "/status", Headers, Options),
    %%lager:info("response = ~p", [ResponseToGet]),
    {ok, Response} = ResponseToGet,

    %% validate correct get response for env not running
    %% breakout the status, headers, body
    #{ status_code := RespStatus
     , headers := RespHeaders
     , body := RespBody
     } = Response,

    %% valididate response code is 200 
    200 = RespStatus,

    %% test header contents are correct
    { <<"server">>, <<"Cowboy">>} =  lists:keyfind( <<"server">>
                                                  , 1
                                                  , RespHeaders
                                                  ),
    { <<"date">>, _Date } =  lists:keyfind( <<"date">>
                                          , 1
                                          , RespHeaders
                                          ),
    { <<"content-type">>, <<"text/html">>} =  lists:keyfind( <<"content-type">>
                                                           , 1
                                                           , RespHeaders
                                                           ),
    { <<"content-length">>, <<"60">>} =  lists:keyfind( <<"content-length">>
                                                      , 1
                                                      , RespHeaders
                                                      ),

    %% valididate body content
    <<"<html><body>Environment Server not running :-(</body></html>">> = RespBody,

    ok.

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

    %% for now command to reply with json of State
    %% decode the json and check for key/values of interest
    ExpectedJsonPairs = [ {<<"sim_type">>, <<"language">>}
                        , {<<"restart">>, 0}
                        ],
    %% look for keys in json, but not values
    ExpectedJsonKeys = [ ],

    %% send request, test response
    helper:send_recieve( post
                , ReqHeaders       % to send
                , Options          % to send
                , ReqBody          % to send
                , Url              % to send
                , ExpectedStatus  % test get this received
                , ExpectedJsonPairs
                , ExpectedJsonKeys
                ),

    ok.

test_init_again(Config)  ->
    %% set up getting status via 'GET' (note env already running)

    Url = "/status",
    Headers = [ {<<"content-type">>, <<"application/text">>} ],
    Options = #{},
    ReqBody = "",

    %% expect to get 200 status code
    ExpectedStatus = 200,

    %% for now command to reply with json of State
    %% decode the json and check for key/values of interest
    ExpectedJsonPairs = [ {<<"sim_type">>, <<"language">>}
                        , {<<"restart">>, 0}
                        , {<<"init_state">>, <<"none">>}
                        ],
    %% look for keys in json, but not values
    ExpectedJsonKeys = [ <<"this_machine">>
                       , <<"svr_map">>
                       , <<"start_time">>
                       ],

    %% send request, test response
    helper:send_recieve( get
                , Headers       % to send
                , Options          % to send
                , ReqBody          % to send
                , Url              % to send
                , ExpectedStatus  % test get this received
                , ExpectedJsonPairs
                , ExpectedJsonKeys
                ),

    ok.
