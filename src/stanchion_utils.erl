%% ---------------------------------------------------------------------
%%
%% Copyright (c) 2007-2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% ---------------------------------------------------------------------

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
         get_manifests/3,
         get_object/2,
         get_object/3,
         has_tombstone/1,
         list_keys/1,
         list_keys/2,
         pow/2,
         pow/3,
         put_object/4,
         riak_connection/0,
         riak_connection/2,
         set_bucket_acl/2,
         set_bucket_policy/2,
         delete_bucket_policy/2,
         timestamp/1,
         to_bucket_name/2]).

-include("stanchion.hrl").
-include_lib("riak_pb/include/riak_pb_kv_codec.hrl").

-define(EMAIL_INDEX, <<"email_bin">>).
-define(ID_INDEX, <<"c_id_bin">>).
-define(OBJECT_BUCKET_PREFIX, <<"0o:">>).       % Version # = 0
-define(BLOCK_BUCKET_PREFIX, <<"0b:">>).        % Version # = 0

-type bucket_op() :: create | update_acl | delete | update_policy | delete_policy.

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
    do_bucket_op(Bucket, OwnerId, {acl, Acl}, create).

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
    do_bucket_op(Bucket, OwnerId, {acl, ?ACL{}}, delete).

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
-spec from_bucket_name(binary()) -> {'blocks' | 'objects', binary()}.
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

%% @doc
-spec get_manifests(pid(), binary(), binary()) ->
    {ok, term(), term()} | {error, notfound}.
get_manifests(RiakcPid, Bucket, Key) ->
    case get_manifests_raw(RiakcPid, Bucket, Key) of
        {ok, Object} ->
            DecodedSiblings = [binary_to_term(V) ||
                                  {_, V}=Content <- riakc_obj:get_contents(Object),
                                  not has_tombstone(Content)],

            %% Upgrade the manifests to be the latest erlang
            %% record version
            Upgraded = stanchion_manifest_utils:upgrade_wrapped_manifests(DecodedSiblings),

            %% resolve the siblings
            Resolved = stanchion_manifest_resolution:resolve(Upgraded),

            %% prune old scheduled_delete manifests

            %% commented out because we don't have the
            %% riak_cs_gc module
            %% Pruned = stanchion_manifest_utils:prune(Resolved),
            {ok, Object, Resolved};
        {error, notfound}=NotFound ->
            NotFound
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

%% @doc Determine if a set of contents of a riak object has a tombstone.
-spec has_tombstone({dict(), binary()}) -> boolean().
has_tombstone({_, <<>>}) ->
    true;
has_tombstone({MD, _V}) ->
    dict:is_key(<<"X-Riak-Deleted">>, MD) =:= true.

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
%% though whole metadata itself is a dict, a metadata of ?MD_USERMETA is
%% proplists of [{?MD_ACL|?MD_POLICY, ACL::binary()|PolicyBin::binary()}].
%% should preserve other metadata. ACL and Policy can be overwritten.
-spec put_bucket(term(), binary(), {acl, acl()}|{policy, binary()}, pid())
                -> ok | {error, term()}.
put_bucket(BucketObj, OwnerId, AclOrPolicy, RiakPid) ->
    PutOptions = [{w, all}, {pw, all}],
    UpdBucketObj0 = riakc_obj:update_value(BucketObj, OwnerId),
    MD = case riakc_obj:get_metadatas(UpdBucketObj0) of
             [] -> % create
                 {acl, Acl} = AclOrPolicy,
                 M0 = [{?MD_ACL, term_to_binary(Acl)}],
                 dict:from_list([{?MD_USERMETA, M0}]);
             [MD0] -> MD0;
             _E ->
                 MsgData = {siblings, riakc_obj:key(BucketObj)},
                 _ = lager:error("bucket has siblings: ~p", [MsgData]),
                 throw(MsgData) % @TODO: data broken; handle this
           end,
    MetaVals = dict:fetch(?MD_USERMETA, MD),
    UserMetaData = make_new_user_metadata(MetaVals, AclOrPolicy),
    MetaData = make_new_metadata(MD, UserMetaData),
    UpdBucketObj = riakc_obj:update_metadata(UpdBucketObj0, MetaData),
    riakc_pb_socket:put(RiakPid, UpdBucketObj, PutOptions).

make_new_metadata(MD, UserMeta) ->
    dict:store(?MD_USERMETA, UserMeta, dict:erase(?MD_USERMETA, MD)).

make_new_user_metadata(MetaVals, {acl, Acl})->
    [{?MD_ACL, term_to_binary(Acl)} | proplists:delete(?MD_ACL, MetaVals)];
make_new_user_metadata(MetaVals, {policy, Policy}) ->
    [{?MD_POLICY, term_to_binary(Policy)} |
     proplists:delete(?MD_POLICY, MetaVals)];
make_new_user_metadata(MetaVals, delete_policy) ->
    proplists:delete(?MD_POLICY, MetaVals).

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

%% @doc Set the ACL for a bucket
-spec set_bucket_acl(binary(), term()) -> ok | {error, term()}.
set_bucket_acl(Bucket, FieldList) ->
    %% @TODO Check for missing fields
    OwnerId = proplists:get_value(<<"requester">>, FieldList, <<>>),
    AclJson = proplists:get_value(<<"acl">>, FieldList, []),
    Acl = stanchion_acl_utils:acl_from_json(AclJson),
    do_bucket_op(Bucket, OwnerId, {acl, Acl}, update_acl).

%% @doc add bucket policy in the global namespace
%% FieldList.policy has JSON-encoded policy from user
-spec set_bucket_policy(binary(), term()) -> ok | {error, term()}.
set_bucket_policy(Bucket, FieldList) ->
    OwnerId = proplists:get_value(<<"requester">>, FieldList, <<>>),
    PolicyJson = proplists:get_value(<<"policy">>, FieldList, []),

    % @TODO: Already Checked at Riak CS, so store as it is JSON here
    % if overhead of parsing JSON were expensive, need to import
    % code of JSON parse from riak_cs_s3_policy
    do_bucket_op(Bucket, OwnerId, {policy, PolicyJson}, update_policy).


%% @doc Delete a bucket
-spec delete_bucket_policy(binary(), binary()) -> ok | {error, term()}.
delete_bucket_policy(Bucket, OwnerId) ->
    do_bucket_op(Bucket, OwnerId, delete_policy, delete_policy).

%% @doc Generate a key for storing a set of manifests for deletion.
-spec timestamp(erlang:timestamp()) -> non_neg_integer().
timestamp({MegaSecs, Secs, _MicroSecs}) ->
    (MegaSecs * 1000000) + Secs.

%% Get the proper bucket name for either the MOSS object
%% bucket or the data block bucket.
-spec to_bucket_name(objects | blocks, binary()) -> binary().
to_bucket_name(Type, Bucket) ->
    case Type of
        objects ->
            Prefix = ?OBJECT_BUCKET_PREFIX;
        blocks ->
            Prefix = ?BLOCK_BUCKET_PREFIX
    end,
    BucketHash = crypto:md5(Bucket),
    <<Prefix/binary, BucketHash/binary>>.

%% ===================================================================
%% Internal functions
%% ===================================================================

%% @doc Check if a bucket is empty
-spec bucket_empty(binary(), pid()) -> boolean().
bucket_empty(Bucket, RiakcPid) ->
    ManifestBucket = to_bucket_name(objects, Bucket),
    %% @TODO Use `stream_list_keys' instead and
    ListKeysResult = list_keys(ManifestBucket, RiakcPid),
    bucket_empty_handle_list_keys(RiakcPid,
                                  Bucket,
                                  ListKeysResult).

-spec bucket_empty_handle_list_keys(pid(), binary(),
                                    {ok, list()} |
                                    {error, term()}) ->
    boolean().
bucket_empty_handle_list_keys(RiakcPid, Bucket, {ok, Keys}) ->
    AnyPred = bucket_empty_any_pred(RiakcPid, Bucket),
    %% `lists:any/2' will break out early as soon
    %% as something returns `true'
    not lists:any(AnyPred, Keys);
bucket_empty_handle_list_keys(_RiakcPid, _Bucket, _Error) ->
    false.

-spec bucket_empty_any_pred(RiakcPid :: pid(), Bucket :: binary()) ->
    fun((Key :: binary()) -> boolean()).
bucket_empty_any_pred(RiakcPid, Bucket) ->
    fun (Key) ->
            key_exists(RiakcPid, Bucket, Key)
    end.

%% @private
%% `Bucket' should be the raw bucket name,
%% we'll take care of calling `to_bucket_name'
-spec key_exists(pid(), binary(), binary()) -> boolean().
key_exists(RiakcPid, Bucket, Key) ->
    key_exists_handle_get_manifests(get_manifests(RiakcPid, Bucket, Key)).

%% @private
-spec key_exists_handle_get_manifests({ok, riakc_obj:riakc_obj(), list()} |
                                      {error, term()}) ->
    boolean().
key_exists_handle_get_manifests({ok, _Object, Manifests}) ->
    active_to_bool(active_manifest_from_response({ok, Manifests}));
key_exists_handle_get_manifests(Error) ->
    active_to_bool(active_manifest_from_response(Error)).

%% @private
-spec active_to_bool({ok, term()} | {error, notfound}) -> boolean().
active_to_bool({ok, _Active}) ->
    true;
active_to_bool({error, notfound}) ->
    false.

-spec active_manifest_from_response({ok, orddict:orddict()} |
                                    {error, notfound}) ->
    {ok, term()} | {error, notfound}.
active_manifest_from_response({ok, Manifests}) ->
    handle_active_manifests(stanchion_manifest_utils:active_manifest(Manifests));
active_manifest_from_response({error, notfound}=NotFound) ->
    NotFound.

%% @private
-spec handle_active_manifests({ok, term()} |
                              {error, no_active_manifest}) ->
    {ok, term()} | {error, notfound}.
handle_active_manifests({ok, _Active}=ActiveReply) ->
    ActiveReply;
handle_active_manifests({error, no_active_manifest}) ->
    {error, notfound}.

%% @doc Determine if a bucket is exists and is available
%% for creation or deletion by the inquiring user.
-spec bucket_available(binary(), fun(), bucket_op(), pid()) -> {true, term()} | {false, atom()}.
bucket_available(Bucket, RequesterId, BucketOp, RiakPid) ->
    GetOptions = [{pr, all}],
    case riakc_pb_socket:get(RiakPid, ?BUCKETS_BUCKET, Bucket, GetOptions) of
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
                OwnerId == RequesterId andalso
                BucketOp == update_policy ->
                    {true, BucketObj};
                OwnerId == RequesterId andalso
                BucketOp == delete_policy ->
                    {true, BucketObj};
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
                update_policy ->
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
-spec do_bucket_op(binary(), binary(),
                   {acl, acl()}|{policy, binary()},
                   bucket_op()) -> ok | {error, term()}.
do_bucket_op(<<"riak-cs">>, _OwnerId, _Acl, _BucketOp) ->
    {error, access_denied};
do_bucket_op(Bucket, OwnerId, AclOrPolicy, BucketOp) ->
    case riak_connection() of
        {ok, RiakPid} ->
            %% Buckets operations can only be completed if the bucket exists
            %% and the requesting party owns the bucket.
            Res = case bucket_available(Bucket, OwnerId, BucketOp, RiakPid) of
                      {true, BucketObj} ->
                          Value = case BucketOp of
                                      create ->        OwnerId;
                                      update_acl ->    OwnerId;
                                      update_policy -> OwnerId;
                                      delete_policy -> OwnerId;
                                      delete ->        ?FREE_BUCKET_MARKER
                                  end,
                          put_bucket(BucketObj, Value, AclOrPolicy, RiakPid);
                      {false, Reason1} ->
                          {error, Reason1}
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

%% internal fun to retrieve the riak object
%% at a bucket/key
-spec get_manifests_raw(pid(), binary(), binary()) ->
    {ok, riakc_obj:riakc_obj()} | {error, notfound}.
get_manifests_raw(RiakcPid, Bucket, Key) ->
    ManifestBucket = to_bucket_name(objects, Bucket),
    riakc_pb_socket:get(RiakcPid, ManifestBucket, Key).

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
