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

-module(stanchion_multipart).

-export([check_no_multipart_uploads/2]).

-include("stanchion.hrl").

-type cluster_id() :: undefined | binary().  % Type still in flux.

-type cs_uuid() :: binary().

-record(lfs_manifest_v2, {
        version=2 :: integer(),
        block_size :: integer(),
        bkey :: {binary(), binary()},
        metadata :: orddict:orddict(),
        created=riak_moss_wm_utils:iso_8601_datetime(),
        uuid :: cs_uuid(),
        content_length :: non_neg_integer(),
        content_type :: binary(),
        content_md5 :: term(),
        state=undefined :: undefined | writing | active |
                           pending_delete | scheduled_delete | deleted,
        write_start_time :: term(), %% immutable
        last_block_written_time :: term(),
        write_blocks_remaining :: ordsets:ordset(integer()),
        delete_marked_time :: term(),
        last_block_deleted_time :: term(),
        delete_blocks_remaining :: ordsets:ordset(integer()),
        acl :: acl(),
        props = [] :: proplists:proplist(),
        cluster_id :: cluster_id()
    }).

-record(lfs_manifest_v3, {
        %% "global" properties
        %% -----------------------------------------------------------------

        %% this isn't as important anymore
        %% since we're naming the record
        %% to include the version number,
        %% but I figured it's worth keeping
        %% in case we change serialization
        %% formats in the future.
        version=3 :: integer(),

        %% the block_size setting when this manifest
        %% was written. Needed if the user
        %% ever changes the block size after writing
        %% data
        block_size :: integer(),

        %% identifying properties
        %% -----------------------------------------------------------------
        bkey :: {binary(), binary()},

        %% user metadata that would normally
        %% be placed on the riak_object. We avoid
        %% putting it on the riak_object so that
        %% we can use that metadata ourselves
        metadata :: orddict:orddict(),

        %% the date the manifest was created.
        %% not sure if we need both this and
        %% write_start_time. My thought was that
        %% write_start_time would have millisecond
        %% resolution, but I suppose there's no
        %% reason we can't change created
        %% to have millisecond as well.
        created=riak_moss_wm_utils:iso_8601_datetime(),
        uuid :: cs_uuid(),

        %% content properties
        %% -----------------------------------------------------------------
        content_length :: non_neg_integer(),
        content_type :: binary(),
        content_md5 :: term(),

        %% state properties
        %% -----------------------------------------------------------------
        state=undefined :: undefined | writing | active |
                           pending_delete | scheduled_delete | deleted,

        %% writing/active state
        %% -----------------------------------------------------------------
        write_start_time :: term(), %% immutable

        %% used for two purposes
        %% 1. to mark when a file has finished uploading
        %% 2. to decide if a write crashed before completing
        %% and needs to be garbage collected
        last_block_written_time :: term(),

        %% a shrink-only (during resolution)
        %% set to denote which blocks still
        %% need to be written. We use a shrinking
        %% (rather than growing) set to that the
        %% set is empty after the write has completed,
        %% which should be most of the lifespan on disk
        write_blocks_remaining :: ordsets:ordset(integer()),

        %% pending_delete/deleted state
        %% -----------------------------------------------------------------
        %% set to the current time
        %% when a manifest is marked as deleted
        %% and enters the pending_delete state
        delete_marked_time :: term(),

        %% the timestamp serves a similar
        %% purpose to last_block_written_time,
        %% in that it's used for figuring out
        %% when delete processes have died
        %% and garbage collection needs to
        %% pick up where they left off.
        last_block_deleted_time :: term(),

        %% a shrink-only (during resolution)
        %% set to denote which blocks
        %% still need to be deleted.
        %% See write_blocks_remaining for
        %% an explanation of why we chose
        %% a shrinking set
        delete_blocks_remaining :: ordsets:ordset(integer()),

        %% the time the manifest was put
        %% into the scheduled_delete
        %% state
        scheduled_delete_time :: term(),

        %% The ACL for the version of the object represented
        %% by this manifest.
        acl :: acl(),

        %% There are a couple of cases where we want to add record
        %% member'ish data without adding new members to the record,
        %% e.g.
        %%    1. Data for which the common value is 'undefined' or not
        %%       used/set for this particular manifest
        %%    2. Cases where we do want to change the structure of the
        %%       record but don't want to go through the full code
        %%       refactoring and backward-compatibility tap dance
        %%       until sometime later.
        props = [] :: proplists:proplist(),

        %% cluster_id: A couple of uses, both short- and longer-term
        %%  possibilities:
        %%
        %%  1. We don't have a good story in early 2012 for how to
        %%     build a stable 2,000 node Riak cluster.  If MOSS can
        %%     talk to multiple Riak clusters, then each individual
        %%     cluster can be a size that we're comfortable
        %%     supporting.
        %%
        %%  2. We may soon have Riak EE's replication have full
        %%     plumbing to make it feasible to forward arbitrary
        %%     traffic between clusters.  Then if a slave cluster is
        %%     missing a data block, and read-repair cannot
        %%     automagically fix the 'not_found' problem, then perhaps
        %%     forwarding a get request to the source Riak cluster can
        %%     fetch us the missing data.
        cluster_id :: cluster_id()
    }).
-type lfs_manifest() :: #lfs_manifest_v3{}.
-define(MANIFEST, #lfs_manifest_v3).

check_no_multipart_uploads(Bucket, RiakPid) ->
    HashBucket = stanchion_utils:to_bucket_name(objects, Bucket),

    {{ok, Keys}, TAT} = ?TURNAROUND_TIME(riakc_pb_socket:list_keys(RiakPid, HashBucket)),
    stanchion_stats:update([riakc, list_all_manifest_keys], TAT),

    %% check all up
    lists:all(fun(Key) ->
                      GetResult = stanchion_utils:get_manifests_raw(RiakPid, Bucket, Key),
                      has_no_upload(GetResult)
              end, Keys).

has_no_upload({ok, Obj}) ->
    Manifests = manifests_from_riak_object(Obj),
    lists:all(fun({_UUID,Manifest}) ->
                      case Manifest?MANIFEST.state of
                          writing ->
                              %% if this is mp => false
                              not proplists:is_defined(multipart, Manifest?MANIFEST.props);
                          _ ->
                              true
                      end
              end, Manifests);
has_no_upload({error, notfound}) -> true;
has_no_upload({error, _} = Error) ->
    _ = logger:error("unexpected error: ~p", [Error]),
    true.

-spec manifests_from_riak_object(riakc_obj:riakc_obj()) -> orddict:orddict().
manifests_from_riak_object(RiakObject) ->
    %% For example, riak_cs_manifest_fsm:get_and_update/4 may wish to
    %% update the #riakc_obj without a roundtrip to Riak first.  So we
    %% need to see what the latest
    Contents = try
                   %% get_update_value will return the updatevalue or
                   %% a single old original value.
                   [{riakc_obj:get_update_metadata(RiakObject),
                     riakc_obj:get_update_value(RiakObject)}]
               catch throw:_ ->
                       %% Original value had many contents
                       riakc_obj:get_contents(RiakObject)
               end,
    DecodedSiblings = [binary_to_term(V) ||
                          {_, V}=Content <- Contents,
                          not stanchion_utils:has_tombstone(Content)],

    %% Upgrade the manifests to be the latest erlang
    %% record version
    Upgraded = upgrade_wrapped_manifests(DecodedSiblings),

    %% resolve the siblings
    stanchion_manifest_resolution:resolve(Upgraded).

-spec upgrade_wrapped_manifests([orddict:orddict()]) -> [orddict:orddict()].
upgrade_wrapped_manifests(ListofOrdDicts) ->
    DictMapFun = fun(_Key, Value) -> upgrade_manifest(Value) end,
    MapFun = fun(Value) -> orddict:map(DictMapFun, Value) end,
    lists:map(MapFun, ListofOrdDicts).

%% @doc Upgrade the manifest to the most recent
%% version of the manifest record. This is so that
%% _most_ of the codebase only has to deal with
%% the most recent version of the record.
-spec upgrade_manifest(lfs_manifest() | #lfs_manifest_v2{}) -> lfs_manifest().
upgrade_manifest(#lfs_manifest_v2{block_size=BlockSize,
                                 bkey=Bkey,
                                 metadata=Metadata,
                                 created=Created,
                                 uuid=UUID,
                                 content_length=ContentLength,
                                 content_type=ContentType,
                                 content_md5=ContentMd5,
                                 state=State,
                                 write_start_time=WriteStartTime,
                                 last_block_written_time=LastBlockWrittenTime,
                                 write_blocks_remaining=WriteBlocksRemaining,
                                 delete_marked_time=DeleteMarkedTime,
                                 last_block_deleted_time=LastBlockDeletedTime,
                                 delete_blocks_remaining=DeleteBlocksRemaining,
                                 acl=Acl,
                                 props=Properties,
                                 cluster_id=ClusterID}) ->

    upgrade_manifest(?MANIFEST{block_size=BlockSize,
                               bkey=Bkey,
                               metadata=Metadata,
                               created=Created,
                               uuid=UUID,
                               content_length=ContentLength,
                               content_type=ContentType,
                               content_md5=ContentMd5,
                               state=State,
                               write_start_time=WriteStartTime,
                               last_block_written_time=LastBlockWrittenTime,
                               write_blocks_remaining=WriteBlocksRemaining,
                               delete_marked_time=DeleteMarkedTime,
                               last_block_deleted_time=LastBlockDeletedTime,
                               delete_blocks_remaining=DeleteBlocksRemaining,
                               acl=Acl,
                               props=Properties,
                               cluster_id=ClusterID});

upgrade_manifest(?MANIFEST{props=Props}=M) ->
    M?MANIFEST{props=fixup_props(Props)}.

-spec fixup_props(undefined | list()) -> list().
fixup_props(undefined) ->
    [];
fixup_props(Props) when is_list(Props) ->
    Props.
