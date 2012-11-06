%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

%% @doc Module to process bucket creation requests.

-module(stanchion_server).

-behaviour(gen_server).

-include("stanchion.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test API
-export([test_link/0]).

-endif.

%% API
-export([start_link/0,
         create_bucket/1,
         create_user/1,
         delete_bucket/2,
         stop/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {riak_ip :: string(),
                riak_port :: pos_integer()}).
-type state() :: #state{}.


%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Start a `stanchion_server'.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Attempt to create a bucket
-spec create_bucket([{term(), term()}]) -> ok | {error, term()}.
create_bucket(BucketData) ->
    gen_server:call(?MODULE, {create_bucket, BucketData}, infinity).

%% @doc Attempt to create a bucket
-spec create_user([{term(), term()}]) ->
                         ok |
                         {error, term()} |
                         {error, stanchion_utils:riak_connect_failed()}.
create_user(UserData) ->
    gen_server:call(?MODULE, {create_user, UserData}, infinity).

%% @doc Attempt to delete a bucket
-spec delete_bucket(binary(), binary()) -> ok | {error, term()}.
delete_bucket(Bucket, UserId) ->
    gen_server:call(?MODULE, {delete_bucket, Bucket, UserId}, infinity).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

%% @doc Initialize the server.
-spec init([] | test) -> {ok, state()}.
init([]) ->
    {ok, #state{}};
init(test) ->
      {ok, #state{}}.

%% @doc Handle synchronous commands issued via exported functions.
-spec handle_call(term(), {pid(), term()}, state()) ->
                         {reply, ok, state()}.
handle_call({create_bucket, BucketData},
            _From,
            State=#state{}) ->
    Result = stanchion_utils:create_bucket(BucketData),
    {reply, Result, State};
handle_call({create_user, UserData},
            _From,
            State=#state{}) ->
    Result = stanchion_utils:create_user(UserData),
    {reply, Result, State};
handle_call({delete_bucket, Bucket, OwnerId},
            _From,
            State=#state{}) ->
    Result = stanchion_utils:delete_bucket(Bucket, OwnerId),
    {reply, Result, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

%% @doc Handle asynchronous commands issued via
%% the exported functions.
-spec handle_cast(term(), state()) ->
                         {noreply, state()}.
handle_cast(list_buckets, State) ->
    %% @TODO Handle bucket listing and reply
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Event, State) ->
    _ = lager:warning("Received unknown cast event: ~p", [Event]),
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

%% @doc Start a `stanchion_server' for testing.
-spec test_link() -> {ok, pid()} | {error, term()}.
test_link() ->
    gen_server:start_link(?MODULE, test, []).

-endif.
