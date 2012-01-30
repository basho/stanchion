%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

%% @doc Convenience functions for setting up the bucket_bouncer HTTP interface.

-module(bucket_bouncer_web).

-export([dispatch_table/0]).

dispatch_table() ->
    case application:get_env(bucket_bouncer, auth_bypass) of
        {ok, AuthBypass} ->
            ok;
        undefined ->
            AuthBypass = false
    end,
    [
     {["buckets"], bucket_bouncer_wm_buckets, [{auth_bypass, AuthBypass}]},
     {["buckets", bucket], bucket_bouncer_wm_bucket, [{auth_bypass, AuthBypass}]}
    ].
