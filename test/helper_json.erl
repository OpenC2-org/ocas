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

%% read a file containing valid json and return json object
read_json_file(Filename) ->
    read_json_file(Filename, _valid=true).

%% read a file containing valid json and return json object
read_json_file(Filename, _valid=true) ->
    Json = read_file(Filename),
    jsx:is_json(Json),
    Json;

%% read a file containing broken json and return it for testing rainyday
read_json_file(Filename, _valid=false) ->
    read_file(Filename).

read_file(Filename) ->
    %% read text from a file
    {ok, Txt} = file:read_file(Filename),
    Txt.

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
