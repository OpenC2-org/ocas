-module(status_ok_handler).

-author("Duncan Sparrell").
-license("Apache 2.0").
%%%-------------------------------------------------------------------
%%% @author Duncan Sparrell
%%% @copyright (C) 2016, sFractal Consulting LLC
%%%
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are
%%% met:
%%%
%%% * Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer.
%%%
%%% * Redistributions in binary form must reproduce the above copyright
%%%   notice, this list of conditions and the following disclaimer in the
%%%   documentation and/or other materials provided with the distribution.
%%%
%%% * The names of its contributors may not be used to endorse or promote
%%%   products derived from this software without specific prior written
%%%   permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------

-export([ init/3
        , rest_init/2
        , allowed_methods/2
        , ok_to_json/2
        , ok_to_text/2
        , ok_to_html/2
        , content_types_provided/2
        ]).

init( {tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {Method, Req1} = cowboy_req:method(Req),
    {URL, Req2} = cowboy_req:url(Req1),
    lager:info("~s ~s", [Method, URL]),
    {ok, Req2, #{}}.

%% allow only GET
allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

%% what will reply with
content_types_provided(Req, State) ->
    {[
        {<<"text/html">>, ok_to_html},
        {<<"application/json">>, ok_to_json},
        {<<"text/plain">>, ok_to_text}
     ] , Req, State}.

ok_to_html(Req, State) ->
    %% more to do - really should check (message a status node?) first
    %% for now just say ok
    Body = <<"<html><body>ok</body></html>">>,

    {Body, Req, State}.

ok_to_text(Req, State) ->
    %% more to do - really should check (message a status node?) first
    %% for now just say ok
    Body = <<"ok">>,

    {Body, Req, State}.

ok_to_json(Req, State) ->
    %% more to do - really should check (message a status node?) first
    %% for now just say ok
    Body = <<"{ \"status : ok\" }">>,

    {Body, Req, State}.
