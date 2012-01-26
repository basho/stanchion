%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

%% @doc Module to process bucket creation requests.

-module(bucket_bouncer_server).

-behaviour(gen_server).

-include("bucket_bouncer.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test API
-export([test_link/0]).

-endif.

%% API
-export([start_link/0,
         create_bucket/3,
         delete_bucket/3,
         stop/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {db_ref :: binary(),
                data_dir :: string(),
                storage_module :: atom()}).
-type state() :: #state{}.


%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Start a `bucket_bouncer_server'.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% @doc Attempt to create a bucket
-spec create_bucket(pid(), binary(), binary()) -> ok | {error, term()}.
create_bucket(Pid, Bucket, UserId) ->
    gen_server:call(Pid, {create_bucket, Bucket, UserId}).

%% @doc Attempt to delete a bucket
-spec delete_bucket(pid(), binary(), binary()) -> ok | {error, term()}.
delete_bucket(Pid, Bucket, UserId) ->
    gen_server:call(Pid, {delete_bucket, Bucket, UserId}).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

%% @doc Initialize the server.
-spec init([] | {test, [atom()]}) -> {ok, state()} | {stop, term()}.
init([StorageMod, DataDir]) ->
    %% Open an eleveldb instance
    case StorageMod:open(DataDir) of
        {ok, Ref} ->
            {ok, #state{db_ref=Ref,
                        data_dir=DataDir,
                        storage_module=StorageMod}};
        {error, Reason} ->
            lager:error("Failed to establish connection to Riak. Reason: ~p",
                        [Reason]),
            {stop, riak_connect_failed}
    end;
init(test) ->
      {ok, #state{storage_module=riak_storage_dummy}}.

%% @doc Handle synchronous commands issued via exported functions.
-spec handle_call(term(), {pid(), term()}, state()) ->
                         {reply, ok, state()}.
handle_call({create_bucket, Bucket, OwnerId},
            From,
            State=#state{db_ref=Ref,
                         storage_module=StorageMod}) ->
    {reply, ok, State};
handle_call({delete_bucket, Bucket, OwnerId},
            From,
            State=#state{db_ref=Ref,
                         storage_module=StorageMod}) ->
    {reply, ok, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

%% @doc Handle asynchronous commands issued via
%% the exported functions.
-spec handle_cast(term(), state()) ->
                         {noreply, state()}.
handle_cast(list_buckets, State=#state{db_ref=Ref,
                                       storage_module=StorageMod}) ->
    %% @TODO Handle bucket listing and reply
    {noreply, State};
handle_cast(stop, State=#state{db_ref=Ref,
                               storage_module=StorageMod}) ->
    StorageMod:close(Ref),
    {stop, normal, State};
handle_cast(Event, State) ->
    lager:warning("Received unknown cast event: ~p", [Event]),
    {noreply, State}.

%% @doc @TODO
-spec handle_info(term(), state()) ->
                         {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Unused.
-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @doc Unused.
-spec code_change(term(), state(), term()) ->
                         {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% ===================================================================
%% Test API
%% ===================================================================

-ifdef(TEST).

%% @doc Start a `bucket_bouncer_server' for testing.
-spec test_link() -> {ok, pid()} | {error, term()}.
test_link() ->
    gen_server:start_link(?MODULE, test, []).

-endif.
