%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

%% @doc Convenience functions for setting up the stanchion HTTP interface.

-module(stanchion_web).

-export([dispatch_table/0]).

dispatch_table() ->
    case application:get_env(stanchion, auth_bypass) of
        {ok, AuthBypass} ->
            ok;
        undefined ->
            AuthBypass = false
    end,
    [
     {["buckets"], stanchion_wm_buckets, [{auth_bypass, AuthBypass}]},
     {["buckets", bucket, "acl"], stanchion_wm_acl, [{auth_bypass, AuthBypass}]},
     {["buckets", bucket], stanchion_wm_bucket, [{auth_bypass, AuthBypass}]},
     {["users"], stanchion_wm_users, [{auth_bypass, AuthBypass}]}
    ].
