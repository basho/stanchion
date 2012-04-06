%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

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
