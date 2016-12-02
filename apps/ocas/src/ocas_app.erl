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
%% @doc ocas public API
%% @end
%%%-------------------------------------------------------------------

-module(ocas_app).
-author("Duncan Sparrell").
-copyright("2016, sFractal Consulting, LLC").
-license(apache2).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

-spec start(_, _) -> {'ok', pid() }.
start(_StartType, _StartArgs) ->

    %% log pid of this
    lager:info("ocas_app pid: ~p", [self()]),

    %% log what apps running
    Apps = application:which_applications(),
    lager:info("apps: ~p", [Apps]),

    %% start supervisor
    lager:info("starting supervisor"),
    ocas_sup:start_link(),

    %% start webserver
    lager:info("starting webserver"),
    WebServerReturn = start_webserver(),

    %% log some info
    lager:info("webserver return: ~p", [WebServerReturn]),
    AppEnv = application:get_all_env(),
    lager:info("env: ~p", [AppEnv]),

    %% return
    {ok, WebServerReturn}.

-spec start() -> {'error', {atom(), _}} | {'ok', [atom()]}.
start() ->
  application:ensure_all_started(ocas).

%%--------------------------------------------------------------------
-spec stop(_) -> 'ok'.
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
-spec start_webserver() -> pid().
start_webserver() ->
  %% which port to listen to
  {ok, Port} = application:get_env(port),
  lager:info("starting cowboy on port: ~p", [Port]),

  %% how many parallel listeners
  {ok, ListenerCount} = application:get_env(listener_count),
  lager:info("starting ~p listeners", [ListenerCount]),

  %% setup routes
  Routes =
    [
      {
        '_'  %virtual hostname (any host name)
      , [
          {"/status", status_handler, []}
        , {"/ok", status_ok_handler, []}  % returns ok if service working
        , {"/openc2", openc2_handler, []}    % handles the meat of openc2
        ]
      }
    ],
  Dispatch = cowboy_router:compile(Routes),

  %% start cowboy
  {ok, CowboyReturn} = cowboy:start_http( http
                             , ListenerCount
                             , [{port, Port}]
                             , [{env, [{dispatch, Dispatch}]}]
                             ),
  lager:info("Cowboy started returned: ~p", [CowboyReturn] ),

  %% return
  self().
