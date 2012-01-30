%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

-module(bucket_bouncer_passthru_auth).

-export([authenticate/2]).

-spec authenticate(term(), [string()]) -> ok.
authenticate(_RD, _) ->
    ok.
