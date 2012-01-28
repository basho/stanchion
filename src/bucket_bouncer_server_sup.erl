%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

-module(bucket_bouncer_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc API for starting the supervisor.
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% @doc Initialize this supervisor. This is a `one_for_one',
%%      whose child spec is for starting a `bucket_bouncer_server' process.
-spec init([]) -> {ok, {{supervisor:strategy(),
                         pos_integer(),
                         pos_integer()},
                        [supervisor:child_spec()]}}.
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = temporary,
    Shutdown = 2000,
    Type = worker,

    ServerSpec = {undefined,
                  {bucket_bouncer_server, start_link, []},
                  Restart, Shutdown, Type, [bucket_bouncer_server]},

    {ok, {SupFlags, [ServerSpec]}}.
