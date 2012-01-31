%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

-define(DEFAULT_TIMEOUT, 60000).

-record(bbc, {ip :: string(),
              port :: pos_integer(),
              ssl :: boolean()}).
-type bbc() :: #bbc{}.
