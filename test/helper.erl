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
-copyright("2016, sFractal Consulting, LLC").
-license(apache2).

%% for test export all functions
-compile(export_all).

%% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%%%%%%%%%%%%%%%%%%%% Utilities

%% utility to save putting this in each test
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
