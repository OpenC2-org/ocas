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
%% @doc helper routines for test
%% @end
%%%-------------------------------------------------------------------

-module(helper_json).
-author("Duncan Sparrell").
-copyright("2017, sFractal Consulting, LLC").
-license(apache2).

%% for test export all functions
-compile(export_all).

%% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%%%%%%%%%%%%%%%%%%%% Utilities

%% utilities to save putting this in each test

%% include path in filename
full_data_file_name(Filename, Config) ->
    filename:join( ?config( data_dir, Config ), Filename ).

%% read a file containing valid json and return json object
read_json_file(Filename, Config) ->
    FullFilename = full_data_file_name(Filename, Config),
    read_json_file(FullFilename).

%% read a file containing valid json and return json object
read_json_file(Filename) ->
    Json = read_file(Filename),
    jsx:is_json(Json),
    Json.

read_file(Filename) ->
    %% read text from a file
    {ok, Txt} = file:read_file(Filename),
    Txt.

%% post openc2 command and get expected result
post_oc2(JsonFileName, ResultsFileName, Config) ->
    %% read and validate json to send
    JsonTxt = read_json_file(JsonFileName, Config),

    %% read and validate expected results
    ExpectedResultsTxt = read_json_file(ResultsFileName, Config),

    %% convert json to erlang terms
    ExpectedResults = jsx:decode(ExpectedResultsTxt, [return_maps]),

    %% open connection to send post
    Conn = make_conn(),

    %% send post
    {ok, Response} = shotgun:post( Conn
                                 , "/openc2"  % Url
                                 , [ { <<"content-type">>  % ReqHeaders
                                     , <<"application/json">>
                                     }
                                   ]
                                 , JsonTxt  % ReqBody
                                 , #{}      % Options
                                 ),
    %% compare expected status to resonse status
    check_status(ExpectedResults, Response),

    %% check headers
    check_headers(Response),

    %% check has body and is json
    #{ body := RespBody } = Response,
    true = jsx:is_json(RespBody),

    %% decode json into erlang map
    JsonMap = jsx:decode( RespBody, [return_maps] ),

    %% check all components are in response json
    check_keys(JsonMap, ExpectedResults),

    %% check correct return values in response
    check_json_values(JsonMap, ExpectedResults),

    ok.

%% make a connection
make_conn() ->
    MyPort = application:get_env(ocas, port, 8080),
    {ok, Conn} = shotgun:open("localhost", MyPort),
    Conn.

%% check status of a response
check_status(ExpectedResults, Response) ->
    ExpectedStatus = maps:get(<<"ExpectedStatus">>, ExpectedResults),
    ResponseStatus = maps:get(status_code, Response),
    ExpectedStatus = ResponseStatus,
    ok.

%% check response headers
check_headers(Response) ->
    %% get the headers out of the response

    #{ headers := RespHeaders } = Response,

    %% verify headers
    { <<"server">>, <<"Cowboy">>} =  lists:keyfind( <<"server">>
                                                  , 1
                                                  , RespHeaders
                                                  ),
    { <<"date">>, _Date } =  lists:keyfind(<<"date">>, 1, RespHeaders),
    ok.

check_keys(JsonMap, ExpectedResults) ->
    %% get expected keys
    ExpectedKeys = maps:get(<<"ExpectedJsonKeys">>, ExpectedResults),

    %% get response keys
    ResponseKeys = maps:keys(JsonMap),
    lager:info("ResponseKeys: ~p", [ResponseKeys]),

    %% check expected are in response
    check_key(ResponseKeys, ExpectedKeys),
    JsonMap.

check_key(_ResultKeys, [] ) ->
    %% done since list empty
    ok;

check_key(ResultKeys, [Key | RestOfKeys] ) ->
    %% check key is in Results
    lager:info("check_key: ~p", [Key]),
    true = lists:member(Key, ResultKeys),

    %% recurse thru rest of list
    check_key(ResultKeys, RestOfKeys).

%% check response contains key/values expected
check_json_values(JsonMap, ExpectedResults) ->
    %% get expected key/value map
    ExpectedJsonPairMap = maps:get(<<"ExpectedJsonPairs">>, ExpectedResults),
    ExpectedJsonPairs = maps:to_list(ExpectedJsonPairMap),
    lager:info("ResponseMap: ~p", [JsonMap]),

    %% recurse thru {key,value} in ExpectedJsonPairs looking for match
    check_json_pair( ExpectedJsonPairs, JsonMap).

check_json_pair( [], _JsonMap) ->
    % done
    ok;

check_json_pair( [ {Key, Value} | RestOfExepectedPairs ], JsonMap) ->
    %% check in key/value in json map
    lager:info("key/value: ~p/~p", [Key, Value]),
    Value = maps:get(Key, JsonMap),

    %% recurse on to next pair
    check_json_pair( RestOfExepectedPairs, JsonMap).

%% make sure all openc2 atoms preexist (for json decoding)
create_openc2_atoms() ->
    command_atoms(),
    action_atoms(),
    actuator_atoms(),
    target_atoms(),
    modifier_atom(),
    ok.

command_atoms() ->
    [ action
    , target
    , actuator
    , modifier
    , specifiers
    ].
action_atoms() ->
    [ allow
    , deny
    ].

actuator_atoms() ->
    [].
target_atoms() ->
    [].
modifier_atom() ->
    [].
