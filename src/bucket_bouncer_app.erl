%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2011 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

%% @doc Callbacks for the bucket_bouncer application.

-module(bucket_bouncer_app).

-behaviour(application).

%% application API
-export([start/2,
         stop/1]).


-type start_type() :: normal | {takeover, node()} | {failover, node()}.
-type start_args() :: term().

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc application start callback for bucket_bouncer.
-spec start(start_type(), start_args()) -> {ok, pid()} |
                                           {error, term()}.
start(_Type, _StartArgs) ->
    case bucket_bouncer_utils:riak_connection() of
        {ok, _} ->
            bucket_bouncer_sup:start_link();
        {error, Reason} ->
            lager:error("Couldn't connect to Riak: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc application stop callback for bucket_bouncer.
-spec stop(term()) -> ok.
stop(_State) ->
    ok.
