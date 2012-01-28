%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

-module(bucket_bouncer_wm_utils).

-export([service_available/2,
         parse_auth_header/2,
         iso_8601_datetime/0]).

-include("bucket_bouncer.hrl").
-include_lib("webmachine/include/webmachine.hrl").

service_available(RD, Ctx) ->
    %% TODO:
    %% At some point in the future
    %% this needs to check if we have
    %% an alive Riak server. Although
    %% maybe it makes more sense to be
    %% optimistic and wait untl we actually
    %% check the ACL?

    %% For now we just always
    %% return true
    {true, RD, Ctx}.

%% @doc Parse an authentication header string and determine
%%      the appropriate module to use to authenticate the request.
%%      The passthru auth can be used either with a KeyID or
%%      anonymously by leving the header empty.
-spec parse_auth_header(string(), boolean()) -> {ok, atom(), [string()]} | {error, term()}.
parse_auth_header(KeyID, true) when KeyID =/= undefined ->
    {ok, bucket_bouncer_passthru_auth, [KeyID]};
parse_auth_header(_, true) ->
    {ok, bucket_bouncer_passthru_auth, []};
parse_auth_header(undefined, false) ->
    {ok, bucket_bouncer_blockall_auth, [unkown_auth_scheme]};
parse_auth_header("AWS " ++ Key, _) ->
    case string:tokens(Key, ":") of
        [KeyId, KeyData] ->
            {ok, bucket_bouncer_s3_auth, [KeyId, KeyData]};
        Other -> Other
    end;
parse_auth_header(_, _) ->
    {ok, bucket_bouncer_blockall_auth, [unkown_auth_scheme]}.

%% @doc Get an ISO 8601 formatted timestamp representing
%% current time.
-spec iso_8601_datetime() -> [non_neg_integer()].
iso_8601_datetime() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = erlang:universaltime(),
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.000Z",
                  [Year, Month, Day, Hour, Min, Sec]).
