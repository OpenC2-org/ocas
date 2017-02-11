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
%% @doc helper routines for test
%% @end
%%%-------------------------------------------------------------------

-module(helper).
-author("Duncan Sparrell").
-copyright("2017, sFractal Consulting, LLC").
-license(apache2).

%% for test export all functions
-compile(export_all).

%% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%%%%%%%%%%%%%%%%%%%% Utilities

%% utilities to save putting this in each test

%% sends http get and expects json response
send_receive( get
            , Url               % to send
            , ReqHeaders        % to send
            , Options           % to send
            , ExpectedStatus    % test get this received
            , ExpectedJsonKeys  % list of expected keys in json
            , ExpectedJsonPairs % list of key/values in json
            ) ->
    Conn = make_conn(),
    {ok, Response} = shotgun:get(Conn, Url, ReqHeaders, Options),
    lager:info("response = ~p", [Response]),

    check_status(Response, ExpectedStatus),

    check_headers(Response),

    check_json(Response, ExpectedJsonKeys, ExpectedJsonPairs),

    ok.

%% sends http post with json and expects json response
send_receive( post
            , Url              % to send
            , ReqHeaders       % to send
            , Options          % to send
            , ReqBody          % to send
            , ExpectedStatus   % test get this received
            , ExpectedJsonKeys  % list of expected keys in json
            , ExpectedJsonPairs % list of key/values in json
            ) ->

    Conn = make_conn(),
    {ok, Response} = shotgun:post(Conn, Url, ReqHeaders, ReqBody, Options),
    lager:info("response = ~p", [Response]),

    check_status(Response, ExpectedStatus),

    check_headers(Response),

    check_json(Response, ExpectedJsonKeys, ExpectedJsonPairs),

    ok.

send_recieve( ReqHeaders          % to send
            , Options          % to send
            , ReqBody          % to send
            , Url              % to send
            , ExpectedStatus  % test get this received
            , ExpectedJsonPairs   % list of key/values in json
            ) ->

    MyPort = application:get_env(ocas, port, 8080),

    {ok, Conn} = shotgun:open("localhost", MyPort),
    {ok, Response} = shotgun:post(Conn, Url, ReqHeaders, ReqBody, Options),
    lager:info("response = ~p", [Response]),

    %% get status code of response
    #{ status_code := RespStatus } = Response,

    %% test what received was what was expected
    ExpectedStatus = RespStatus,

    %% get headers
    #{ headers := RespHeaders } = Response,
    %%lager:info("headers = ~p", [RespHeaders]),

    %% verify headers
    { <<"server">>, <<"Cowboy">>} =  lists:keyfind( <<"server">>
                                                  , 1
                                                  , RespHeaders
                                                  ),
    { <<"date">>, _Date } =  lists:keyfind(<<"date">>, 1, RespHeaders),

    %% check if has body
    #{ body := RespBody } = Response,

    %% check body is json
    true = jsx:is_json(RespBody),

    %% decode json into erlang map
    JsonMap = jsx:decode( RespBody, [return_maps] ),

    lager:info("ExpectedJsonPairs: ~p", [ExpectedJsonPairs]),
    lager:info("JsonMap: ~p", [JsonMap]),

    %% check key/value pairs are as expected
    check_map(ExpectedJsonPairs, JsonMap),

    %% return
    ok.


make_conn() ->
    MyPort = application:get_env(ocas, port, 8080),

    {ok, Conn} = shotgun:open("localhost", MyPort),

    Conn.

check_status(Response, ExpectedStatus) ->
    %% get status code of response
    #{ status_code := RespStatus } = Response,

    %% test what received was what was expected
    ExpectedStatus = RespStatus,
    ok.

check_headers(Response) ->
    %% get headers
    #{ headers := RespHeaders } = Response,
    %%lager:info("headers = ~p", [RespHeaders]),

    %% verify headers
    { <<"server">>, <<"Cowboy">>} =  lists:keyfind( <<"server">>
                                                  , 1
                                                  , RespHeaders
                                                  ),
    { <<"date">>, _Date } =  lists:keyfind(<<"date">>, 1, RespHeaders),
    ok.

check_json(Response, ExpectedJsonKeys, ExpectedJsonPairs) ->
    %% check if has body
    #{ body := RespBody } = Response,

    %% check body is json
    true = jsx:is_json(RespBody),

    %% decode json into erlang map
    JsonMap = jsx:decode( RespBody, [return_maps] ),

    lager:info("ExpectedJsonPairs: ~p", [ExpectedJsonPairs]),
    lager:info("JsonMap: ~p", [JsonMap]),

    %% check keys are as expected
    check_keys(ExpectedJsonKeys, JsonMap),

    %% check key/value pairs are as expected
    check_map(ExpectedJsonPairs, JsonMap),
    ok.

check_keys( [], _JsonMap ) ->
    %% done since list is empty
    ok;

check_keys( [Key | RestOfKeys], JsonMap ) ->
    %% check key is in JsonMap
    lager:info("Testing Key: ~p", [Key]),
    true = maps:is_key(Key, JsonMap),
    check_keys(RestOfKeys, JsonMap).

check_map( [], _JsonMap ) ->
    %% done since list is empty
    ok;

check_map( [ {Key, ExpectedValue} | RestOfExpectedJsonPairs ], JsonMap ) ->
    %% Grab  first item in list and verify
    lager:info("Testing Key/ExpectedValue: ~p/~p", [Key, ExpectedValue]),

    %% see if key is in JsonMap
    %%    error formating added so can tell which key/value failed
    case maps:is_key(Key, JsonMap) of
        true ->
            %% key exists so check value
            case maps:get(Key, JsonMap) of
                ExpectedValue ->
                    %% key exists and value correct so go to next
                    check_map( RestOfExpectedJsonPairs, JsonMap);
                BadValue ->
                    %% key exists but has wrong value so format test fail error
                    ErrorText = list_to_binary(
                                   io_lib:format( "~p/~p = bad value for /key"
                                                , [BadValue, Key]
                                                )
                                   ),
                    %% force test fail with ErrorText as cause
                    cause_failure = ErrorText
             end;
        false ->
            %% key doesn't exist so format test fail error
            ErrorText = list_to_binary(
                                io_lib:format( "~p key not found in json"
                                             , [Key]
                                             )
                                ),
            %% force test fail with ErrorText as cause
            cause_failure = ErrorText
    end.

check_key( [], _JsonMap ) ->
    %% done since list is empty
    ok;

check_key( [ Key | RestOfExpectedKeys ], JsonMap ) ->
    %% Grab  first item in list and verify
    maps:is_key(Key, JsonMap),
    %% recurse
    check_key( RestOfExpectedKeys, JsonMap ).
