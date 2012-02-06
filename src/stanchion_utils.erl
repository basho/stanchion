%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

%% @doc stanchion utility functions

-module(stanchion_utils).

%% Public API
-export([binary_to_hexlist/1,
         close_riak_connection/1,
         create_bucket/2,
         delete_bucket/2,
         delete_object/2,
         from_bucket_name/1,
         get_admin_creds/0,
         get_buckets/1,
         get_keys_and_values/1,
         get_object/2,
         get_object/3,
         list_keys/1,
         list_keys/2,
         pow/2,
         pow/3,
         put_object/4,
         riak_connection/0,
         riak_connection/2,
         to_bucket_name/2]).

-include("stanchion.hrl").

-define(OBJECT_BUCKET_PREFIX, <<"objects:">>).
-define(BLOCK_BUCKET_PREFIX, <<"blocks:">>).

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Convert the passed binary into a string where the numbers are represented in hexadecimal (lowercase and 0 prefilled).
-spec binary_to_hexlist(binary()) -> string().
binary_to_hexlist(Bin) ->
    XBin =
        [ begin
              Hex = erlang:integer_to_list(X, 16),
              if
                  X < 16 ->
                      lists:flatten(["0" | Hex]);
                  true ->
                      Hex
              end
          end || X <- binary_to_list(Bin)],
    string:to_lower(lists:flatten(XBin)).

%% @doc Close a protobufs connection to the riak cluster.
-spec close_riak_connection(pid()) -> ok.
close_riak_connection(Pid) ->
    riakc_pb_socket:stop(Pid).

%% @doc Create a bucket in the global namespace or return
%% an error if it already exists.
-spec create_bucket(binary(), binary()) -> ok | {error, term()}.
create_bucket(Bucket, OwnerId) ->
    do_bucket_op(Bucket, OwnerId, create).

%% @doc Delete a bucket
-spec delete_bucket(binary(), binary()) -> ok | {error, term()}.
delete_bucket(Bucket, OwnerId) ->
    do_bucket_op(Bucket, OwnerId, delete).

%% @doc Delete an object from Riak
-spec delete_object(binary(), binary()) -> ok.
delete_object(BucketName, Key) ->
    case riak_connection() of
        {ok, RiakPid} ->
            Res = riakc_pb_socket:delete(RiakPid, BucketName, Key),
            close_riak_connection(RiakPid),
            Res;
        {error, Reason} ->
            {error, {riak_connect_failed, Reason}}
    end.

%% Get the root bucket name for either a MOSS object
%% bucket or the data block bucket name.
-spec from_bucket_name(binary()) -> binary().
from_bucket_name(BucketNameWithPrefix) ->
    BlocksName = ?BLOCK_BUCKET_PREFIX,
    ObjectsName = ?OBJECT_BUCKET_PREFIX,
    BlockByteSize = byte_size(BlocksName),
    ObjectsByteSize = byte_size(ObjectsName),

    case BucketNameWithPrefix of
        <<BlocksName:BlockByteSize/binary, BucketName/binary>> ->
            {blocks, BucketName};
        <<ObjectsName:ObjectsByteSize/binary, BucketName/binary>> ->
            {objects, BucketName}
    end.

%% @doc Return the credentials of the admin user
-spec get_admin_creds() -> {ok, {string(), string()}} | {error, term()}.
get_admin_creds() ->
    case application:get_env(stanchion, admin_key) of
        {ok, KeyId} ->
            case application:get_env(stanchion, admin_secret) of
                {ok, Secret} ->
                    {ok, {KeyId, Secret}};
                undefined ->
                    lager:warning("The admin user's secret has not been defined."),
                    {error, secret_undefined}
            end;
        undefined ->
            lager:warning("The admin user's key id has not been defined."),
            {error, key_id_undefined}
    end.

%% @doc Return a user's buckets.
-spec get_buckets(all | binary()) -> [binary()].
get_buckets(<<>>) ->
    get_keys_and_values(?BUCKETS_BUCKET);
get_buckets(OwnerId) ->
    case get_keys_and_values(?BUCKETS_BUCKET) of
        {ok, KeyValuePairs} ->
            [{Key, Value} || {Key, Value} <- KeyValuePairs,
                           Value == OwnerId];
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Get an object from Riak
-spec get_object(binary(), binary()) ->
                        {ok, binary()} | {error, term()}.
get_object(BucketName, Key) ->
    case riak_connection() of
        {ok, RiakPid} ->
            Res = get_object(BucketName, Key, RiakPid),
            close_riak_connection(RiakPid),
            Res;
        {error, Reason} ->
            {error, {riak_connect_failed, Reason}}
    end.

%% @doc Get an object from Riak
-spec get_object(binary(), binary(), pid()) ->
                        {ok, binary()} | {error, term()}.
get_object(BucketName, Key, RiakPid) ->
    riakc_pb_socket:get(RiakPid, BucketName, Key).

%% @doc List the keys from a bucket
-spec list_keys(binary()) -> {ok, [binary()]}.
list_keys(BucketName) ->
    case riak_connection() of
        {ok, RiakPid} ->
            Res = list_keys(BucketName, RiakPid),
            close_riak_connection(RiakPid),
            Res;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc List the keys from a bucket
-spec list_keys(binary(), pid()) -> {ok, [binary()]}.
list_keys(BucketName, RiakPid) ->
    case riakc_pb_socket:list_keys(RiakPid, BucketName) of
        {ok, Keys} ->
            {ok, lists:sort(Keys)};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Integer version of the standard pow() function; call the recursive accumulator to calculate.
-spec pow(integer(), integer()) -> integer().
pow(Base, Power) ->
    pow(Base, Power, 1).

%% @doc Integer version of the standard pow() function.
-spec pow(integer(), integer(), integer()) -> integer().
pow(Base, Power, Acc) ->
    case Power of
        0 ->
            Acc;
        _ ->
            pow(Base, Power - 1, Acc * Base)
    end.

%% @doc Store a new bucket in Riak
-spec put_bucket(binary(), binary(), pid()) -> ok | {error, term()}.
put_bucket(Bucket, OwnerId, RiakPid) ->
    BucketObject = riakc_obj:new(?BUCKETS_BUCKET, Bucket, OwnerId),
    riakc_pb_socket:put(RiakPid, BucketObject).

%% @doc Store an object in Riak
-spec put_object(binary(), binary(), binary(), [term()]) -> ok.
put_object(BucketName, Key, Value, Metadata) ->
    case riak_connection() of
        {ok, RiakPid} ->
            RiakObject = riakc_obj:new(BucketName, Key, Value),
            NewObj = riakc_obj:update_metadata(RiakObject, Metadata),
            Res = riakc_pb_socket:put(RiakPid, NewObj),
            close_riak_connection(RiakPid),
            Res;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Get a protobufs connection to the riak cluster
%% using information from the application environment.
-spec riak_connection() -> {ok, pid()} | {error, term()}.
riak_connection() ->
    case application:get_env(stanchion, riak_ip) of
        {ok, Host} ->
            ok;
        undefined ->
            Host = "127.0.0.1"
    end,
    case application:get_env(stanchion, riak_pb_port) of
        {ok, Port} ->
            ok;
        undefined ->
            Port = 8087
    end,
    riak_connection(Host, Port).

%% @doc Get a protobufs connection to the riak cluster.
-spec riak_connection(string(), pos_integer()) -> {ok, pid()} | {error, term()}.
riak_connection(Host, Port) ->
    riakc_pb_socket:start_link(Host, Port).

%% Get the proper bucket name for either the MOSS object
%% bucket or the data block bucket.
-spec to_bucket_name([objects | blocks], binary()) -> binary().
to_bucket_name(objects, Name) ->
    <<?OBJECT_BUCKET_PREFIX/binary, Name/binary>>;
to_bucket_name(blocks, Name) ->
    <<?BLOCK_BUCKET_PREFIX/binary, Name/binary>>.

%% ===================================================================
%% Internal functions
%% ===================================================================

%% @doc Check if a bucket is empty
-spec bucket_empty(binary(), pid()) -> boolean().
bucket_empty(Bucket, RiakPid) ->
    ObjBucket = to_bucket_name(objects, Bucket),
    case list_keys(ObjBucket, RiakPid) of
        {ok, []} ->
            true;
        _ ->
            false
    end.

%% @doc Determine if a bucket is exists and is available
%% for creation or deletion by the inquiring user.
-spec bucket_available(binary(), fun(), atom(), pid()) -> true | {false, atom()}.
bucket_available(Bucket, RequesterId, BucketOp, RiakPid) ->
    case riakc_pb_socket:get(RiakPid, ?BUCKETS_BUCKET, Bucket) of
        {ok, BucketObj} ->
            OwnerId = riakc_obj:get_value(BucketObj),
            if
                OwnerId == ?FREE_BUCKET_MARKER andalso
                BucketOp == create ->
                    true;
                OwnerId == ?FREE_BUCKET_MARKER andalso
                BucketOp == delete ->
                    {false, no_such_bucket};
                OwnerId == RequesterId andalso
                BucketOp == create ->
                    true;
                OwnerId == RequesterId andalso
                BucketOp == delete ->
                    case bucket_empty(Bucket, RiakPid) of
                        true ->
                            true;
                        false ->
                            {false, bucket_not_empty}
                    end;
                true ->
                    {false, bucket_already_exists}
            end;
        {error, notfound} ->
            case BucketOp of
                create ->
                    true;
                delete ->
                    {false, no_such_bucket}
            end;
        {error, Reason} ->
            %% @TODO Maybe bubble up this error info
            lager:warning("Error occurred trying to check if the bucket ~p exists. Reason: ~p", [Bucket, Reason]),
            {false, Reason}
    end.

%% @doc Perform an operation on a bucket.
-spec do_bucket_op(binary(), binary(), atom()) -> ok | {error, term()}.
do_bucket_op(Bucket, OwnerId, BucketOp) ->
    case riak_connection() of
        {ok, RiakPid} ->
            %% Buckets operations can only be completed if the bucket exists
            %% and the requesting party owns the bucket.
            case bucket_available(Bucket, OwnerId, BucketOp, RiakPid) of
                true ->
                    case BucketOp of
                        create ->
                            Value = OwnerId;
                        delete ->
                            Value = ?FREE_BUCKET_MARKER
                    end,
                    Res = put_bucket(Bucket, Value, RiakPid);
                {false, Reason1} ->
                    Res = {error, Reason1}
            end,
            close_riak_connection(RiakPid),
            Res;
        {error, Reason} ->
            {error, {riak_connect_failed, Reason}}
    end.

%% @doc Return a list of keys for a bucket along
%% with their associated values
-spec get_keys_and_values(binary()) -> {ok, [{binary(), binary()}]}.
get_keys_and_values(BucketName) ->
    case riak_connection() of
        {ok, RiakPid} ->
            case list_keys(BucketName, RiakPid) of
                {ok, Keys} ->
                    KeyValuePairs =
                        [{Key, get_value(BucketName, Key, RiakPid)}
                         || Key <- Keys],
                    Res = {ok, KeyValuePairs};
                {error, Reason1} ->
                    Res = {error, Reason1}
            end,
            close_riak_connection(RiakPid),
            Res;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Extract the value from a Riak object.
-spec get_value(binary(), binary(), pid()) ->
                        {ok, binary()} | {error, term()}.
get_value(BucketName, Key, RiakPid) ->
    case get_object(BucketName, Key, RiakPid) of
        {ok, RiakObj} ->
            riakc_obj:get_value(RiakObj);
        {error, Reason} ->
            lager:warning("Failed to retrieve value for ~p. Reason: ~p", [Key, Reason]),
            <<"unknown">>
    end.
