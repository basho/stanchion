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
         create_bucket/1,
         create_user/1,
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
         set_bucket_acl/2,
         to_bucket_name/2]).
-export([do_bucket_op/4]). %SLF experimental thing

-include("stanchion.hrl").
-include_lib("riak_pb/include/riak_pb_kv_codec.hrl").

-define(EMAIL_INDEX, <<"email_bin">>).
-define(ID_INDEX, <<"c_id_bin">>).
-define(OBJECT_BUCKET_PREFIX, <<"0o:">>).       % Version # = 0
-define(BLOCK_BUCKET_PREFIX, <<"0b:">>).        % Version # = 0

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
-spec create_bucket([{term(), term()}]) -> ok | {error, term()}.
create_bucket(BucketFields) ->
    %% @TODO Check for missing fields
    Bucket = proplists:get_value(<<"bucket">>, BucketFields, <<>>),
    OwnerId = proplists:get_value(<<"requester">>, BucketFields, <<>>),
    AclJson = proplists:get_value(<<"acl">>, BucketFields, []),
    Acl = stanchion_acl_utils:acl_from_json(AclJson),
    do_bucket_op(Bucket, OwnerId, Acl, create).

%% @doc Attmpt to create a new user
-spec create_user([{term(), term()}]) -> ok | {error, riak_connect_failed() | term()}.
create_user(UserFields) ->
    %% @TODO Check for missing fields
    UserName = binary_to_list(proplists:get_value(<<"name">>, UserFields, <<>>)),
    DisplayName = binary_to_list(proplists:get_value(<<"display_name">>, UserFields, <<>>)),
    Email = proplists:get_value(<<"email">>, UserFields, <<>>),
    KeyId = binary_to_list(proplists:get_value(<<"key_id">>, UserFields, <<>>)),
    KeySecret = binary_to_list(proplists:get_value(<<"key_secret">>, UserFields, <<>>)),
    CanonicalId = binary_to_list(proplists:get_value(<<"canonical_id">>, UserFields, <<>>)),
    case riak_connection() of
        {ok, RiakPid} ->
            case email_available(Email, RiakPid) of
                true ->
                    User = ?MOSS_USER{name=UserName,
                                      display_name=DisplayName,
                                      email=binary_to_list(Email),
                                      key_id=KeyId,
                                      key_secret=KeySecret,
                                      canonical_id=CanonicalId},
                    Res = save_user(User, RiakPid);
                {false, Reason1} ->
                    Res = {error, Reason1}
            end,
            close_riak_connection(RiakPid),
            Res;
        {error, _} = Else ->
            Else
    end.

%% @doc Delete a bucket
-spec delete_bucket(binary(), binary()) -> ok | {error, term()}.
delete_bucket(Bucket, OwnerId) ->
    do_bucket_op(Bucket, OwnerId, ?ACL{}, delete).

%% @doc Delete an object from Riak
-spec delete_object(binary(), binary()) -> ok.
delete_object(BucketName, Key) ->
    case riak_connection() of
        {ok, RiakPid} ->
            Res = riakc_pb_socket:delete(RiakPid, BucketName, Key),
            close_riak_connection(RiakPid),
            Res;
        {error, _} = Else ->
            Else
    end.

%% Get the root bucket name for either a MOSS object
%% bucket or the data block bucket name.
-spec from_bucket_name(binary()) -> {blocks | objects, binary()}.
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
                    _ = lager:warning("The admin user's secret has not been defined."),
                    {error, secret_undefined}
            end;
        undefined ->
            _ = lager:warning("The admin user's key id has not been defined."),
            {error, key_id_undefined}
    end.

%% @doc Return a user's buckets.
-spec get_buckets(all | binary()) -> {ok, [{binary(), binary()}]} | {error, term()}.
get_buckets(<<>>) ->
    get_keys_and_values(?BUCKETS_BUCKET);
get_buckets(OwnerId) ->
    case get_keys_and_values(?BUCKETS_BUCKET) of
        {ok, KeyValuePairs} ->
            {ok, [{Key, Value} || {Key, Value} <- KeyValuePairs,
                                  Value == OwnerId]};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Get an object from Riak
-spec get_object(binary(), binary()) ->
                        {ok, riakc_obj:riakc_obj()} | {error, term()}.
get_object(BucketName, Key) ->
    case riak_connection() of
        {ok, RiakPid} ->
            Res = get_object(BucketName, Key, RiakPid),
            close_riak_connection(RiakPid),
            Res;
        {error, _} = Else ->
            Else
    end.

%% @doc Get an object from Riak
-spec get_object(binary(), binary(), pid()) ->
                        {ok, riakc_obj:riakc_obj()} | {error, term()}.
get_object(BucketName, Key, RiakPid) ->
    riakc_pb_socket:get(RiakPid, BucketName, Key).

%% @doc List the keys from a bucket
-spec list_keys(binary()) -> {ok, [binary()]} | {error, term()}.
list_keys(BucketName) ->
    case riak_connection() of
        {ok, RiakPid} ->
            Res = list_keys(BucketName, RiakPid),
            close_riak_connection(RiakPid),
            Res;
        {error, _} = Else ->
            Else
    end.

%% @doc List the keys from a bucket
-spec list_keys(binary(), pid()) -> {ok, [binary()]} | {error, term()}.
list_keys(BucketName, RiakPid) ->
    case riakc_pb_socket:list_keys(RiakPid, BucketName) of
        {ok, Keys} ->
            {ok, lists:sort(Keys)};
        {error, _} = Else ->
            Else
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
-spec put_bucket(term(), binary(), acl(), pid()) -> ok | {error, term()}.
put_bucket(BucketObj, OwnerId, Acl, RiakPid) ->
    MetaData  = dict:from_list(
                  [{?MD_USERMETA, [{?MD_ACL, term_to_binary(Acl)}]}]),
    UpdBucketObj0 = riakc_obj:update_value(BucketObj, OwnerId),
    UpdBucketObj = riakc_obj:update_metadata(UpdBucketObj0, MetaData),
    riakc_pb_socket:put(RiakPid, UpdBucketObj).

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
        {error, _} = Else ->
            Else
    end.

%% @doc Get a protobufs connection to the riak cluster
%% using information from the application environment.
-type riak_connect_failed() :: {riak_connect_failed, tuple()}.
-spec riak_connection() -> {ok, pid()} | {error, riak_connect_failed()}.
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
-spec riak_connection(string(), pos_integer()) -> {ok, pid()} | {error, riak_connect_failed()}.
riak_connection(Host, Port) ->
    %% We use start() here instead of start_link() because if we can't
    %% connect to Host & Port for whatever reason (e.g. service down,
    %% host down, host unreachable, ...), then we'll be crashed by the
    %% newly-spawned-gen_server-proc's link to us.
    %%
    %% There is still a race condition if the PB socket proc's init()
    %% is successful but then dies immediately *before* we call the
    %% link() BIF.  That's life in the big city.
    case riakc_pb_socket:start(Host, Port) of
        {ok, Pid} = Good ->
            true = link(Pid),
            Good;
        {error, Else} ->
            {error, {riak_connect_failed, {Else, Host, Port}}}
    end.

%% @doc Create a bucket in the global namespace or return
%% an error if it already exists.
-spec set_bucket_acl(binary(), term()) -> ok | {error, term()}.
set_bucket_acl(Bucket, FieldList) ->
    %% @TODO Check for missing fields
    OwnerId = proplists:get_value(<<"requester">>, FieldList, <<>>),
    AclJson = proplists:get_value(<<"acl">>, FieldList, []),
    Acl = stanchion_acl_utils:acl_from_json(AclJson),
    do_bucket_op(Bucket, OwnerId, Acl, update_acl).

%% Get the proper bucket name for either the MOSS object
%% bucket or the data block bucket.
-spec to_bucket_name(objects | blocks, binary()) -> binary().
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
    ManifestBucket = to_bucket_name(objects, Bucket),
    %% @TODO Use `stream_list_keys' instead and
    %% break out as soon as an active manifest is found.
    case list_keys(ManifestBucket, RiakPid) of
        {ok, Keys} ->
            FoldFun =
                fun(Key, Acc) ->
                        {ok, ManiPid} = stanchion_manifest_fsm:start_link(Bucket, Key),
                        case stanchion_manifest_fsm:get_active_manifest(ManiPid) of
                            {ok, _} ->
                                [Key | Acc];
                            {error, notfound} ->
                                Acc
                        end
                end,
            ActiveKeys = lists:foldl(FoldFun, [], Keys),
            case ActiveKeys of
                [] ->
                    true;
                _ ->
                    false
            end;
        _ ->
            false
    end.

%% @doc Determine if a bucket is exists and is available
%% for creation or deletion by the inquiring user.
-spec bucket_available(binary(), fun(), atom(), pid()) -> {true, term()} | {false, atom()}.
bucket_available(Bucket, RequesterId, BucketOp, RiakPid) ->
    case riakc_pb_socket:get(RiakPid, ?BUCKETS_BUCKET, Bucket) of
        {ok, BucketObj} ->
            OwnerId = riakc_obj:get_value(BucketObj),
            if
                OwnerId == ?FREE_BUCKET_MARKER andalso
                BucketOp == create ->
                    {true, BucketObj};
                OwnerId == ?FREE_BUCKET_MARKER andalso
                (BucketOp == delete
                 orelse
                 BucketOp == update_acl) ->
                    {false, no_such_bucket};
                (OwnerId == RequesterId andalso
                BucketOp == create)
                orelse
                BucketOp == update_acl ->
                    {true, BucketObj};
                OwnerId == RequesterId andalso
                BucketOp == delete ->
                    case bucket_empty(Bucket, RiakPid) of
                        true ->
                            {true, BucketObj};
                        false ->
                            {false, bucket_not_empty}
                    end;
                true ->
                    {false, bucket_already_exists}
            end;
        {error, notfound} ->
            case BucketOp of
                create ->
                    BucketObj = riakc_obj:new(?BUCKETS_BUCKET, Bucket, RequesterId),
                    {true, BucketObj};
                update_acl ->
                    {false, no_such_bucket};
                delete ->
                    {false, no_such_bucket}
            end;
        {error, Reason} ->
            %% @TODO Maybe bubble up this error info
            _ = lager:warning("Error occurred trying to check if the bucket ~p exists. Reason: ~p", [Bucket, Reason]),
            {false, Reason}
    end.

%% @doc Perform an operation on a bucket.
-spec do_bucket_op(binary(), binary(), acl(), atom()) -> ok | {error, term()}.
do_bucket_op(<<"riak-cs">>, _OwnerId, _Acl, _BucketOp) ->
    {error, access_denied};
do_bucket_op(Bucket, OwnerId, Acl, BucketOp) ->
    case riak_connection() of
        {ok, RiakPid} ->
            %% Buckets operations can only be completed if the bucket exists
            %% and the requesting party owns the bucket.
            case bucket_available(Bucket, OwnerId, BucketOp, RiakPid) of
                {true, BucketObj} ->
                    case BucketOp of
                        create ->
                            Value = OwnerId;
                        update_acl ->
                            Value = OwnerId;
                        delete ->
                            Value = ?FREE_BUCKET_MARKER
                    end,
                    Res = put_bucket(BucketObj, Value, Acl, RiakPid);
                {false, Reason1} ->
                    Res = {error, Reason1}
            end,
            close_riak_connection(RiakPid),
            Res;
        {error, _} = Else ->
            Else
    end.

%% @doc Determine if a user with the specified email
%% address already exists. There could be consistency
%% issues here since secondary index queries use
%% coverage and only consult a single vnode
%% for a particular key.
%% @TODO Consider other options that would give more
%% assurance that a particular email address is available.
-spec email_available(binary(), pid()) -> true | {false, term()}.
email_available(Email, RiakPid) ->
    case riakc_pb_socket:get_index(RiakPid, ?USER_BUCKET, ?EMAIL_INDEX, Email) of
        {ok, []} ->
            true;
        {ok, _} ->
            {false, user_already_exists};
        {error, Reason} ->
            %% @TODO Maybe bubble up this error info
            _ = lager:warning("Error occurred trying to check if the address ~p has been registered. Reason: ~p", [Email, Reason]),
            {false, Reason}
    end.

%% @doc Return a list of keys for a bucket along
%% with their associated values
-spec get_keys_and_values(binary()) -> {ok, [{binary(), binary()}]} | {error, term()}.
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
        {error, _} = Else ->
            Else
    end.

%% @doc Extract the value from a Riak object.
-spec get_value(binary(), binary(), pid()) ->
                       binary().
get_value(BucketName, Key, RiakPid) ->
    case get_object(BucketName, Key, RiakPid) of
        {ok, RiakObj} ->
            riakc_obj:get_value(RiakObj);
        {error, Reason} ->
            _ = lager:warning("Failed to retrieve value for ~p. Reason: ~p", [Key, Reason]),
            <<"unknown">>
    end.

%% @doc Save information about a user
-spec save_user(moss_user(), pid()) -> ok.
save_user(User, RiakPid) ->
    Indexes = [{?EMAIL_INDEX, User?MOSS_USER.email},
               {?ID_INDEX, User?MOSS_USER.canonical_id}],
    Meta = dict:store(?MD_INDEX, Indexes, dict:new()),
    Obj = riakc_obj:new(?USER_BUCKET, iolist_to_binary(User?MOSS_USER.key_id), term_to_binary(User)),
    UserObj = riakc_obj:update_metadata(Obj, Meta),
    %% @TODO Error handling
    riakc_pb_socket:put(RiakPid, UserObj).
