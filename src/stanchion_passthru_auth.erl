%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

-module(stanchion_passthru_auth).

-export([authenticate/2]).

-spec authenticate(term(), [string()]) -> ok.
authenticate(_RD, _) ->
    ok.
