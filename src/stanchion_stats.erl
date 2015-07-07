%% ---------------------------------------------------------------------
%%
%% Copyright (c) 2007-2015 Basho Technologies, Inc.  All Rights Reserved.
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

-module(stanchion_stats).

%% API
-export([safe_update/2,
         update/2,
         update/3,
         update_with_start/2,
         report_json/0,
         %% report_pretty_json/0,
         get_stats/0]).

-export([init/0]).

-type metric_name() :: list(atom()).
-export_type([metric_name/0]).

-define(METRICS,
        %% [{metric_name(), exometer:type(), [exometer:option()], Aliases}]
        [{[bucket, create], spiral, [],
          [{one, bucket_creates}, {count, bucket_creates_total}]},
         {[bucket, delete], spiral, [],
          [{one, bucket_deletes}, {count, bucket_deletes_total}]},
         {[bucket, put_acl], spiral, [],
          [{one, bucket_put_acl}, {count, bucket_put_acl_total}]},

         {[bucket, create, time], histogram, [],
          [{mean  , bucket_create_time_mean},
           {median, bucket_create_time_median},
           {95    , bucket_create_time_95},
           {99    , bucket_create_time_99},
           {100   , bucket_create_time_100}]},
         {[bucket, delete, time], histogram, [],
          [{mean  , bucket_delete_time_mean},
           {median, bucket_delete_time_median},
           {95    , bucket_delete_time_95},
           {99    , bucket_delete_time_99},
           {100   , bucket_delete_time_100}]},
         {[bucket, put_acl, time], histogram, [],
          [{mean  , bucket_put_acl_time_mean},
           {median, bucket_put_acl_time_median},
           {95    , bucket_put_acl_time_95},
           {99    , bucket_put_acl_time_99},
           {100   , bucket_put_acl_time_100}]},

         {[user, create], spiral, [],
          [{one, user_create}, {count, user_create_total}]},
         {[user, update], spiral, [],
          [{one, user_update}, {count, user_update_total}]},

         {[user, create, time], histogram, [],
          [{mean  , user_create_time_mean},
           {median, user_create_time_median},
           {95    , user_create_time_95},
           {99    , user_create_time_99},
           {100   , user_create_time_100}]},
         {[user, update, time], histogram, [],
          [{mean  , user_update_time_mean},
           {median, user_update_time_median},
           {95    , user_update_time_95},
           {99    , user_update_time_99},
           {100   , user_update_time_100}]},
         
         {[waiting, time], histogram, [],
          [{mean  , waiting_time_mean},
           {median, waiting_time_median},
           {95    , waiting_time_95},
           {99    , waiting_time_99},
           {100   , waiting_time_100}]}
        ]).

%% ====================================================================
%% API
%% ====================================================================



-spec safe_update(metric_name(), integer()) -> ok | {error, any()}.
safe_update(BaseId, ElapsedUs) ->
    %% Just in case those metrics happen to be not registered; should
    %% be a bug and also should not interrupt handling requests by
    %% crashing.
    try
        update(BaseId, ElapsedUs)
    catch T:E ->
            lager:error("Failed on storing some metrics: ~p,~p", [T,E])
    end.

-spec update(metric_name(), integer()) -> ok | {error, any()}.
update(BaseId, ElapsedUs) ->
    ok = exometer:update([stanchion|BaseId], 1),
    ok = exometer:update([stanchion|BaseId]++[time], ElapsedUs).

update(BaseId, ServiceTime, WaitingTime) ->
    update(BaseId, ServiceTime),
    ok = exometer:update([stanchion, waiting, time], WaitingTime).

-spec update_with_start(metric_name(), erlang:timestamp()) ->
                                   ok | {error, any()}.
update_with_start(BaseId, StartTime) ->
    update(BaseId, timer:now_diff(os:timestamp(), StartTime)).

-spec report_json() -> string().
report_json() ->
    lists:flatten(mochijson2:encode({struct, get_stats()})).

-spec get_stats() -> proplists:proplist().
get_stats() ->
    Stats1 = [raw_report_item(I) || I <- ?METRICS],
    Stats2 = [{stanchion_server_msg_q_len, stanchion_server:msg_q_len()}],
    lists:flatten(Stats2 ++ Stats1).

init() ->
    _ = [init_item(I) || I <- ?METRICS],
    ok.

%% ====================================================================
%% Internal
%% ====================================================================

init_item({Name, Type, Opts, Aliases}) ->
    ok = exometer:re_register([stanchion|Name], Type,
                              [{aliases, Aliases}|Opts]).

raw_report_item({Name, _Type, _Options, Aliases}) ->

    {ok, Values} = exometer:get_value([stanchion|Name], [D||{D,_Alias}<-Aliases]),
    [{Alias, Value} ||
        {{D, Alias}, {D, Value}} <- lists:zip(Aliases, Values)].

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

stats_metric_test() ->
    [begin
         ?debugVal(Key),
         case lists:last(Key) of
             time ->
                 ?assertEqual(histogram, Type),
                 [?assert(proplists:is_defined(M, Aliases))
                  || M <- [mean, median, 95, 99, 100]];
             _ ->
                 ?assertNotEqual(false, lists:keyfind(Key, 1, ?METRICS)),
                 ?assertEqual(spiral, Type),
                 ?assert(proplists:is_defined(one, Aliases)),
                 ?assert(proplists:is_defined(count, Aliases))
         end,
         ?assertEqual([], Options)
     end || {Key, Type, Options, Aliases} <- ?METRICS].

stats_test_() ->
    Apps = [setup, compiler, syntax_tools, goldrush, lager, exometer_core],
    {setup,
     fun() ->
             [ok = application:start(App) || App <- Apps],
             ok = stanchion_stats:init()
     end,
     fun(_) ->
             [ok = application:stop(App) || App <- Apps]
     end,
     [{inparallel, [fun() ->
                            %% ?debugVal(Key),
                            case lists:last(Key) of
                                time -> ok;
                                _ -> stanchion_stats:update(Key, 16#deadbeef, 16#deadbeef)
                            end
                    end || {Key, _, _, _} <- ?METRICS]},
     fun() ->
             [begin
                  Items = raw_report_item(I),
                  ?debugVal(Items),
                  case length(Items) of
                      2 ->
                          ?assertEqual([1, 1],
                                       [N || {_, N} <- Items]);
                      5 ->
                          %% TODO: some_metric_100 also should be 16#deadebeef
                          %% something is wrong now, also in riak_cs
                          ?assertEqual([16#deadbeef, 16#deadbeef, 16#deadbeef, 16#deadbeef, 0],
                                       [N || {_, N} <- Items])
                  end
              end || I <- ?METRICS]
     end]}.

-endif.
