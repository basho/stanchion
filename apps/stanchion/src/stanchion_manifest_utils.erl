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

%% @doc Module for choosing and manipulating lists (well, orddict) of manifests

-module(stanchion_manifest_utils).

-include("riak_moss.hrl").
-ifdef(TEST).
-compile(export_all).
-compile(nowarn_export_all).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% export Public API
-export([new_dict/2,
         active_manifest/1,
         active_and_writing_manifests/1,
         overwritten_UUIDs/1,
         mark_pending_delete/2,
         mark_scheduled_delete/2,
         upgrade_wrapped_manifests/1,
         upgrade_manifest/1]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Return a new orddict of manifest (only
%% one in this case). Used when storing something
%% in Riak when the previous GET returned notfound,
%% so we're (maybe) creating a new object.
-spec new_dict(binary(), lfs_manifest()) -> orddict:orddict().
new_dict(UUID, Manifest) ->
    orddict:store(UUID, Manifest, orddict:new()).

%% @doc Return the current active manifest
%% from an orddict of manifests.
-spec active_manifest(orddict:orddict()) -> {ok, lfs_manifest()} | {error, no_active_manifest}.
active_manifest(Dict) ->
    case lists:foldl(fun most_recent_active_manifest/2, no_active_manifest, orddict_values(Dict)) of
        no_active_manifest ->
            {error, no_active_manifest};
        Manifest ->
            {ok, Manifest}
    end.

%% @doc Return a list of all manifests in the
%% `active' or `writing' state
-spec active_and_writing_manifests(orddict:orddict()) -> [lfs_manifest()].
active_and_writing_manifests(Dict) ->
    orddict:to_list(filter_manifests_by_state(Dict, [active, writing])).

%% @doc Extract all manifests that are not "the most active"
%%      and not actively writing (within the leeway period).
-spec overwritten_UUIDs(orddict:orddict()) -> term().
overwritten_UUIDs(Dict) ->
    case active_manifest(Dict) of
        {error, no_active_manifest} ->
            FoldFun =
                fun ({_, ?MANIFEST{state=State}}, Acc) when State =:= writing ->
                        Acc;
                    ({UUID, _}, Acc) ->
                        [UUID | Acc]
                end;
        {ok, Active} ->
            FoldFun =
                fun ({UUID, Elem}, Acc) ->
                        case Elem of
                            Active ->
                                Acc;
                            Elem=?MANIFEST{state=active} ->
                                [UUID | Acc];
                            Elem=?MANIFEST{state=writing} ->
                                [UUID | Acc];
                            _ ->
                                Acc
                        end
                end
    end,
    lists:foldl(FoldFun, [], orddict:to_list(Dict)).

%% @doc Return `Dict' with the manifests in
%% `UUIDsToMark' with their state changed to
%% `pending_delete'
-spec mark_pending_delete(orddict:orddict(), list(binary())) ->
    orddict:orddict().
mark_pending_delete(Dict, UUIDsToMark) ->
    MapFun = fun(K, V) ->
            case lists:member(K, UUIDsToMark) of
                true ->
                    V?MANIFEST{state=pending_delete,
                               delete_marked_time=os:timestamp()};
                false ->
                    V
            end
    end,
    orddict:map(MapFun, Dict).

%% @doc Return `Dict' with the manifests in
%% `UUIDsToMark' with their state changed to
%% `scheduled_delete'
-spec mark_scheduled_delete(orddict:orddict(), list(binary())) ->
    orddict:orddict().
mark_scheduled_delete(Dict, UUIDsToMark) ->
    MapFun = fun(K, V) ->
            case lists:member(K, UUIDsToMark) of
                true ->
                    V?MANIFEST{state=scheduled_delete,
                               scheduled_delete_time=os:timestamp()};
                false ->
                    V
            end
    end,
    orddict:map(MapFun, Dict).

-spec upgrade_wrapped_manifests([orddict:orddict()]) -> [orddict:orddict()].
upgrade_wrapped_manifests(ListofOrdDicts) ->
    DictMapFun = fun (_Key, Value) -> upgrade_manifest(Value) end,
    MapFun = fun (Value) -> orddict:map(DictMapFun, Value) end,
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

    ?MANIFEST{block_size=BlockSize,
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
              cluster_id=ClusterID};

upgrade_manifest(?MANIFEST{}=M) ->
    M.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Filter an orddict manifests and accept only manifests whose
%% current state is specified in the `AcceptedStates' list.
-spec filter_manifests_by_state(orddict:orddict(), [atom()]) -> orddict:orddict().
filter_manifests_by_state(Dict, AcceptedStates) ->
    AcceptManifest =
        fun (_, ?MANIFEST{state=State}) ->
                lists:member(State, AcceptedStates)
        end,
    orddict:filter(AcceptManifest, Dict).

orddict_values(OrdDict) ->
    [V || {_K, V} <- orddict:to_list(OrdDict)].

%% NOTE: This is a foldl function, initial acc = no_active_manifest
most_recent_active_manifest(Manifest=?MANIFEST{state=active}, no_active_manifest) ->
    Manifest;
most_recent_active_manifest(_Manfest, no_active_manifest) ->
    no_active_manifest;
most_recent_active_manifest(Man1=?MANIFEST{state=active}, Man2=?MANIFEST{state=active}) ->
    case Man1?MANIFEST.write_start_time > Man2?MANIFEST.write_start_time of
        true -> Man1;
        false -> Man2
    end;
most_recent_active_manifest(Man1=?MANIFEST{state=active}, _Man2) -> Man1;
most_recent_active_manifest(_Man1, Man2=?MANIFEST{state=active}) -> Man2.
