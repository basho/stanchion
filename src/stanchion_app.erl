%% ---------------------------------------------------------------------
%%
%% Copyright (c) 2007-2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% ---------------------------------------------------------------------

%% @doc Callbacks for the stanchion application.

-module(stanchion_app).

-behaviour(application).

%% application API
-export([start/2,
         stop/1]).


-type start_type() :: normal | {takeover, node()} | {failover, node()}.
-type start_args() :: term().

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc application start callback for stanchion.
-spec start(start_type(), start_args()) -> {ok, pid()} |
                                           {error, term()}.
start(_Type, _StartArgs) ->
    case stanchion_utils:riak_connection() of
        {ok, _} ->
            stanchion_sup:start_link();
        {error, Reason} ->
            _ = lager:error("Couldn't connect to Riak: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc application stop callback for stanchion.
-spec stop(term()) -> ok.
stop(_State) ->
    ok.
