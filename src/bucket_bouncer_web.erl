%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

%% @doc Convenience functions for setting up the bucket_bouncer HTTP interface.

-module(bucket_bouncer_web).

-export([dispatch_table/0]).

dispatch_table() ->
    [
     {[buckets], bucket_bouncer_wm_buckets, []}
    ].
