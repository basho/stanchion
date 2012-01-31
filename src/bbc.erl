%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

%% @doc Client module for interacting with `bucket_bouncer' application.

-module(bbc).

-export([create_bucket/5,
         delete_bucket/5,
         list_buckets/3,
         list_buckets/4,
         ping/3
         %% @TODO update_bucket/3
        ]).

%% @TODO Remove after module development is completed
-export([stats_url/3,
         list_buckets_url/4,
         request/4]).

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Create a bucket for a requesting party.
-spec create_bucket(string(),
                    pos_integer(),
                    binary(),
                    binary(),
                    [{atom(), term()}]) -> ok | {error, term()}.
create_bucket(Ip, Port, Bucket, Requester, Options) ->
    Ssl = proplists:get_value(ssl, Options, true),
    Url = buckets_url(Ip, Port, Ssl),
    Body= "name=" ++
        binary_to_list(Bucket) ++
        "&requester=" ++
        binary_to_list(Requester),
    case request(post, Url, ["204"], [], Body) of
        {ok, "204", _Headers, _ResponseBody} ->
            ok;
        {error, {ok, StatusCode, _Headers, ResponseBody}} ->
            {error, {error_status, StatusCode, ResponseBody}};
        {error, Error} ->
            {error, Error}
    end.

%% @doc Delete a bucket. The bucket must be owned by
%% the requesting party.
-spec delete_bucket(string(),
                    pos_integer(),
                    binary(),
                    binary(),
                    [{atom(), term()}]) -> ok | {error, term()}.
delete_bucket(Ip, Port, Bucket, Requester, Options) ->
    Ssl = proplists:get_value(ssl, Options, true),
    Url = bucket_url(Ip, Port, Ssl, Bucket),
    Body = "requester=" ++ binary_to_list(Requester),
    case request(delete, Url, ["204"], [], Body) of
        {ok, "204", _Headers, _} ->
            ok;
        {error, {ok, StatusCode, _Headers, ResponseBody}} ->
            {error, {error_status, StatusCode, ResponseBody}};
        {error, Error} ->
            {error, Error}
    end.

%% @doc List all the buckets that currently have owners.
-spec list_buckets(string(), pos_integer(), boolean()) -> {ok, [{binary(), binary()}]} | {error, term()}.
list_buckets(_Ip, _Port, _Ssl) ->
    {ok, []}.

%% @doc List all the buckets owned by a particular user.
-spec list_buckets(string(), pos_integer(), boolean(), binary()) -> {ok, [{binary(), binary()}]} | {error, term()}.
list_buckets(_Ip, _Port, _Ssl, _UserId) ->
    {ok, []}.

%% @doc Ping the server by requesting the "/ping" resource.
-spec ping(string(), pos_integer(), boolean()) -> ok | {error, term()}.
ping(Ip, Port, Ssl) ->
    Url = ping_url(Ip, Port, Ssl),
    lager:debug("Ping URL: ~p~n", [Url]),
    case request(get, Url, ["200","204"]) of
        {ok, _Status, _Headers, _Body} ->
            ok;
        {error, Error} ->
            {error, Error}
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

%% @doc Assemble the root URL for the given client
-spec root_url(string(), pos_integer(), boolean()) -> [string()].
root_url(Ip, Port, true) ->
    ["https://", Ip, ":", integer_to_list(Port), "/"];
root_url(Ip, Port, false) ->
    ["http://", Ip, ":", integer_to_list(Port), "/"].

%% @doc Assemble the URL for the ping resource
-spec ping_url(string(), pos_integer(), boolean()) -> string().
ping_url(Ip, Port, Ssl) ->
    lists:flatten([root_url(Ip, Port, Ssl), "ping/"]).

%% @doc Assemble the URL for the stats resource
-spec stats_url(string(), pos_integer(), boolean()) -> string().
stats_url(Ip, Port, Ssl) ->
    lists:flatten([root_url(Ip, Port, Ssl), "stats/"]).

%% @doc Assemble the buckets URL
-spec buckets_url(string(), pos_integer(), boolean()) -> string().
buckets_url(Ip, Port, Ssl) ->
    lists:flatten([root_url(Ip, Port, Ssl), "/buckets"]).

%% @doc Assemble the URL of a bucket
-spec bucket_url(string(), pos_integer(), boolean(), binary()) ->
                               string().
bucket_url(Ip, Port, Ssl, Bucket) ->
    lists:flatten(
      [root_url(Ip, Port, Ssl),
       "/buckets/",
       binary_to_list(Bucket)
      ]).

%% @doc Assemble the URL for the given bucket and key
-spec list_buckets_url(string(), pos_integer(), boolean(), binary()) -> string().
list_buckets_url(Ip, Port, Ssl, Owner) ->
    Query =
        "owner=" ++
        binary_to_list(Owner),
    lists:flatten(
      [root_url(Ip, Port, Ssl),
       "/buckets",
       ["?", mochiweb_util:quote_plus(Query)]
      ]).

%% @doc send an ibrowse request
request(Method, Url, Expect) ->
    request(Method, Url, Expect, [], []).
request(Method, Url, Expect, Headers) ->
    request(Method, Url, Expect, Headers, []).
request(Method, Url, Expect, Headers, Body) ->
    Accept = {"Accept", "multipart/mixed, */*;q=0.9"},
    case ibrowse:send_req(Url, [Accept|Headers], Method, Body,
                          [{response_format, binary}]) of
        Resp={ok, Status, _, _} ->
            case lists:member(Status, Expect) of
                true -> Resp;
                false -> {error, Resp}
            end;
        Error ->
            Error
    end.
