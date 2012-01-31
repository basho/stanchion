%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

%% @doc Client module for interacting with `bucket_bouncer' application.

-module(bbc).

-include("bbc.hrl").

-export([create/0,
         create/3,
         ip/1,
         port/1,
         ping/1,
         create_bucket/3,
         delete_bucket/3,
         list_buckets/1,
         list_buckets/2
        ]).

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Create a client for connecting to `bucket_bouncer' on the
%% default port on localhost.
-spec create() -> bbc().
create() ->
    create("127.0.0.1", 8090, false).

%% @doc Create a client for connecting to a `bucket_bouncer' node.
%%
%%      Connections are made to:
%%      ```http://IP:Port/[buckets[/<bucket>]])'''
%%      or
%%      ```https://IP:Port/[buckets[/<bucket>]])'''
-spec create(string(), pos_integer(), boolean()) -> bbc().
create(IP, Port, Ssl) when is_list(IP),
                           is_integer(Port),
                           is_atom(Ssl) ->
    #bbc{ip=IP, port=Port, ssl=Ssl}.

%% @doc Get the IP this client will connect to.
-spec ip(bbc()) -> string().
ip(#bbc{ip=IP}) -> IP.

%% @doc Get the Port this client will connect to.
-spec port(bbc()) -> integer().
port(#bbc{port=Port}) -> Port.

%% @doc Ping the server by requesting the "/ping" resource.
-spec ping(bbc()) -> ok | {error, term()}.
ping(BBC) ->
    Url = ping_url(BBC),
    lager:debug("Ping URL: ~p~n", [Url]),
    case request(get, Url, ["200","204"]) of
        {ok, _Status, _Headers, _Body} ->
            ok;
        {error, Error} ->
            {error, Error}
    end.

%% @doc Create a bucket for a requesting party.
-spec create_bucket(bbc(), binary(), binary()) -> ok | {error, term()}.
create_bucket(_BBC, _Bucket, _Requester) ->
    ok.

%% @doc Delete a bucket. The bucket must be owned by
%% the requesting party.
-spec delete_bucket(bbc(), binary(), binary()) -> ok | {error, term()}.
delete_bucket(_BBC, _Bucket, _Requester) ->
    ok.

%% @doc List all the buckets that currently have owners.
-spec list_buckets(bbc()) -> {ok, [{binary(), binary()}]} | {error, term()}.
list_buckets(_BBC) ->
    {ok, []}.

%% @doc List all the buckets owned by a particular user.
-spec list_buckets(bbc(), binary()) -> {ok, [{binary(), binary()}]} | {error, term()}.
list_buckets(_BBC, _UserId) ->
    {ok, []}.

%% @doc Delete the given key from the given bucket.
%%
%%      Allowed options are:
%%      <dl>
%%        <dt>`rw'</dt>
%%          <dd>The 'RW' value to use for the delete</dd>
%%      </dl>
%% @spec delete(bbc(), bucket(), key(), proplist()) -> ok|{error, term()}
%% delete(BBC, Bucket, Key, Options) ->
%%     Qs = delete_q_params(BBC, Options),
%%     Url = make_url(BBC, Bucket, Key, Qs),
%%     Headers = [{?HEAD_CLIENT, client_id(BBC, Options)}],
%%     case request(delete, Url, ["204"], Headers) of
%%         {ok, "204", _Headers, _Body} -> ok;
%%         {error, Error}               -> {error, Error}
%%     end.

%% @doc Get the properties of the given bucket.
%% @spec get_bucket(bbc(), bucket()) -> {ok, proplist()}|{error, term()}
%% get_bucket(BBC, Bucket) ->
%%     Url = make_url(BBC, Bucket, undefined, [{?Q_KEYS, ?Q_FALSE}]),
%%     case request(get, Url, ["200"]) of
%%         {ok, "200", _Headers, Body} ->
%%             {struct, Response} = mochijson2:decode(Body),
%%             {struct, Props} = proplists:get_value(?JSON_PROPS, Response),
%%             {ok, bbc_bucket:erlify_props(Props)};
%%         {error, Error} ->
%%             {error, Error}
%%     end.

%% @doc Set the properties of the given bucket.
%%
%%      Allowed properties are:
%%      <dl>
%%        <dt>`n_val'</dt>
%%          <dd>The 'N' value to use for storing data in this bucket</dd>
%%        <dt>`allow_mult'</dt>
%%          <dd>Whether or not this bucket should allow siblings to
%%          be created for its keys</dd>
%%      </dl>
%% @spec set_bucket(bbc(), bucket(), proplist()) -> ok|{error, term()}
%% set_bucket(BBC, Bucket, Props0) ->
%%     Url = make_url(BBC, Bucket, undefined, []),
%%     Headers =  [{"Content-Type", "application/json"}],
%%     Props = bbc_bucket:httpify_props(Props0),
%%     Body = mochijson2:encode({struct, [{?Q_PROPS, {struct, Props}}]}),
%%     case request(put, Url, ["204"], Headers, Body) of
%%         {ok, "204", _Headers, _Body} -> ok;
%%         {error, Error}               -> {error, Error}
%%     end.


%% ===================================================================
%% Internal functions
%% ===================================================================

%% @doc Assemble the root URL for the given client
-spec root_url(bbc()) -> iolist().
root_url(#bbc{ip=Ip, port=Port, ssl=true}) ->
    ["https://", Ip, ":", integer_to_list(Port), "/"];
root_url(#bbc{ip=Ip, port=Port, ssl=false}) ->
    ["http://", Ip, ":", integer_to_list(Port), "/"].

%% @doc Assemble the URL for the ping resource
-spec ping_url(bbc()) -> iolist().
ping_url(BBC) ->
    binary_to_list(iolist_to_binary([root_url(BBC), "ping/"])).

%% @doc Assemble the URL for the stats resource
-spec stats_url(bbc()) -> iolist().
stats_url(BBC) ->
    binary_to_list(iolist_to_binary([root_url(BBC), "stats/"])).

%% @doc Assemble the URL for the given bucket and key
%% @spec make_url(bbc(), bucket(), key(), proplist()) -> iolist()
make_url(BBC, Bucket, Key, Query) ->
    binary_to_list(
      iolist_to_binary(
        [root_url(BBC),
         Bucket, "/",
         [ [Key,"/"] || Key =/= undefined ],
         [ ["?", mochiweb_util:urlencode(Query)] || Query =/= [] ]
        ])).

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

%% @doc stream an ibrowse request
request_stream(Pid, Method, Url) ->
    request_stream(Pid, Method, Url, []).
request_stream(Pid, Method, Url, Headers) ->
    request_stream(Pid, Method, Url, Headers, []).
request_stream(Pid, Method, Url, Headers, Body) ->
    case ibrowse:send_req(Url, Headers, Method, Body,
                          [{stream_to, Pid},
                           {response_format, binary}]) of
        {ibrowse_req_id, ReqId} ->
            {ok, ReqId};
        Error ->
            Error
    end.

%% @doc Convert a stats-resource response to an erlang-term server
%%      information proplist.
erlify_server_info(Props) ->
    lists:flatten([ erlify_server_info(K, V) || {K, V} <- Props ]).
erlify_server_info(<<"nodename">>, Name) -> {node, Name};
erlify_server_info(<<"riak_kv_version">>, Vsn) -> {server_version, Vsn};
erlify_server_info(_Ignore, _) -> [].
