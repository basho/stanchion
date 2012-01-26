%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

%% @doc Utility functions for interacting with `eleveldb'.

-module(bucket_bouncer_eleveldb_utils).

%% Public API
-export([open/1,
         close/1,
         get/3,
         put/4,
         delete/3,
         fold_objects/1]).

-type db_ref() :: binary().
-type read_options() :: [{verify_checksums, boolean()} |
                         {fill_cache, boolean()}].
-type write_options() :: [{sync, boolean()}].

%% @doc Open an eleveldb instance that writes to the specified data diretory.
-spec open(string()) -> db_ref().
open(DataDir) ->
    %% Get the data root directory
    filelib:ensure_dir(filename:join(DataDir, "dummy")),

    WriteBufferSize = config_value(write_buffer_size),

    Options = [
               {create_if_missing, true},
               {write_buffer_size, WriteBufferSize},
               {max_open_files, config_value(max_open_files, 20)},
               {cache_size, config_value(cache_size)},
               {paranoid_checks, config_value(paranoid_checks, false)}
              ],

    lager:debug("Opening LevelDB in ~s with options: ~p\n", [DataDir, Options]),
    eleveldb:open(DataDir, Options).

%% @doc Close an eleveldb instance
-spec close(db_ref()) -> ok.
close(_Ref) ->
    %% No-op; GC handles cleanup
    ok.

%% @doc Retrieve an object from the eleveldb backend
-spec get(db_ref(), binary(), read_options()) ->
                 {ok, any()} |
                 {error, not_found} |
                 {error, term()}.
get(Ref, Key, ReadOpts) ->
    case eleveldb:get(Ref, Key, ReadOpts) of
        not_found  ->
            {error, not_found};
        Value ->
            Value
    end.

%% @doc Insert an object into the eleveldb backend.
-spec put(db_ref(), binary(), binary(), write_options()) -> ok | {error, term()}.
put(Ref, Key, Value, WriteOpts) ->
    eleveldb:write(Ref, [{put, Key, Value}], WriteOpts).

%% @doc Delete an object from the eleveldb backend
-spec delete(db_ref(), binary(), write_options()) -> ok | {error, term()}.
delete(Ref, Key, WriteOpts) ->
    eleveldb:write(Ref, [{delete, Key}], WriteOpts).

%% @doc Fold over all the objects
-spec fold_objects(db_ref()) -> {ok, any()}.
fold_objects(Ref) ->
    FoldFun = fold_objects_fun(),
    try
        eleveldb:fold(Ref, FoldFun, [], [])
    catch
        {break, AccFinal} ->
            AccFinal
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

%% @private
config_value(Key) ->
    config_value(Key, undefined).

%% @private
config_value(Key, Default) ->
    application:get_env(eleveldb, Key, Default).

%% @private
%% @doc Return a function to fold over the objects on this backend
fold_objects_fun() ->
    fun({Key, Value}, Acc) ->
            [{Key, Value} | Acc]
    end.
