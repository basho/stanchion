%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

-module(stanchion_blockall_auth).

-export([authenticate/2]).

-spec authenticate(term(), [string()]) -> {error, atom()}.
authenticate(_RD, [Reason]) ->
    {error, Reason}.
