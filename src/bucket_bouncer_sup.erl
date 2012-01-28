%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

%% @doc Supervisor for the `bucket_bouncer' application.

-module(bucket_bouncer_sup).

-behaviour(supervisor).

%% Public API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

-type startlink_err() :: {'already_started', pid()} | 'shutdown' | term().
-type startlink_ret() :: {'ok', pid()} | 'ignore' | {'error', startlink_err()}.

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc API for starting the supervisor.
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc supervisor callback.
-spec init([]) -> {ok, {{supervisor:strategy(),
                         integer(),
                         integer()},
                        [supervisor:child_spec()]}}.
init([]) ->
    case application:get_env(bucket_bouncer, bb_ip) of
        {ok, Ip} ->
            ok;
        undefined ->
            Ip = "0.0.0.0"
    end,
    case application:get_env(bucket_bouncer, bb_port) of
        {ok, Port} ->
            ok;
        undefined ->
            Port = 80
    end,

    %% Create child specifications
    WebConfig1 = [
                 {dispatch, bucket_bouncer_web:dispatch_table()},
                 {ip, Ip},
                 {port, Port},
                 {nodelay, true},
                 {log_dir, "log"},
                 {rewrite_module, bucket_bouncer_wm_rewrite},
                 {error_handler, bucket_bouncer_wm_error_handler}],
    case application:get_env(bucket_bouncer, ssl) of
        {ok, SSLOpts} ->
            WebConfig = WebConfig1 ++ [{ssl, true},
                                       {ssl_opts, SSLOpts}];
        undefined ->
            WebConfig = WebConfig1
    end,
    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, dynamic},
    ServerSup = {bucket_bouncer_server_sup,
                 {bucket_bouncer_server_sup, start_link, []},
                 permanent, 5000, worker, dynamic},
    Processes = [ServerSup, Web],
    {ok, { {one_for_one, 10, 10}, Processes} }.
