%%%-------------------------------------------------------------------
%%% Copyright (c) 2016, sFractal Consulting

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
%% @doc ocas top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ocas_sup).
-author("Duncan Sparrell").
-copyright("2016, sFractal Consulting, LLC").
-license(apache2).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% macro defining "SERVER" to contain the name of this module
-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
-spec init([]) -> {'ok', {#{}, [#{}, ...]}}.
init([]) ->
    {ok, { {one_for_all, 0, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================
