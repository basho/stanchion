%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

-module(bucket_bouncer_wm_buckets).

-export([init/1,
         service_available/2,
         is_authorized/2,
         content_types_provided/2,
         malformed_request/2,
         to_xml/2,
         allowed_methods/2,
         process_post/2]).

-include("bucket_bouncer.hrl").
-include_lib("webmachine/include/webmachine.hrl").


init(Config) ->
    %% Check if authentication is disabled and
    %% set that in the context.
    AuthBypass = proplists:get_value(auth_bypass, Config),
    {ok, #context{auth_bypass=AuthBypass}}.

-spec service_available(term(), term()) -> {true, term(), term()}.
service_available(RD, Ctx) ->
    bucket_bouncer_wm_utils:service_available(RD, Ctx).

-spec malformed_request(term(), term()) -> {false, term(), term()}.
malformed_request(RD, Ctx) ->
    {false, RD, Ctx}.

%% @doc Check that the request is from the admin user
is_authorized(RD, Ctx=#context{auth_bypass=AuthBypass}) ->
    AuthHeader = wrq:get_req_header("authorization", RD),
    case bucket_bouncer_wm_utils:parse_auth_header(AuthHeader, AuthBypass) of
        {ok, AuthMod, Args} ->
            lager:info("AuthMod: ~p~nArgs: ~p~n", [AuthMod, Args]),
            case AuthMod:authenticate(RD, Args) of
                ok ->
                    %% Authentication succeeded
                    {true, RD, Ctx};
                {error, _Reason} ->
                    %% Authentication failed, deny access
                    bucket_bouncer_response:api_error(access_denied, RD, Ctx)
            end
    end.

%% @doc Get the list of methods this resource supports.
-spec allowed_methods(term(), term()) -> {[atom()], term(), term()}.
allowed_methods(RD, Ctx) ->
    {['GET', 'POST'], RD, Ctx}.

-spec content_types_provided(term(), term()) ->
                                    {[{string(), atom()}], term(), term()}.
content_types_provided(RD, Ctx) ->
    %% @TODO Add JSON support
    {[{"application/xml", to_xml}], RD, Ctx}.

-spec to_xml(term(), term()) ->
                    {iolist(), term(), term()}.
to_xml(RD, Ctx) ->
    OwnerId = list_to_binary(wrq:get_qs_value("owner", "", RD)),
    case bucket_bouncer_utils:get_buckets(OwnerId) of
        {ok, BucketData} ->
            bucket_bouncer_response:list_buckets_response(BucketData,
                                                          RD,
                                                          Ctx);
        {error, Reason} ->
            bucket_bouncer_response:api_error(Reason, RD, Ctx)
    end.

%% @doc Create a user from a POST
%%      and return the user object
%%      as JSON
-spec process_post(term(), term()) -> {true | {halt, pos_integer()}, term(), term()}.
process_post(ReqData, Ctx) ->
    Body = mochiweb_util:parse_qs(
             binary_to_list(
               wrq:req_body(ReqData))),
    Bucket = list_to_binary(proplists:get_value("name", Body, "")),
    RequesterId = list_to_binary(proplists:get_value("requester", Body, "")),
    lager:debug("Bucket: ~p Requester: ~p", [Bucket, RequesterId]),
    case bucket_bouncer_server:create_bucket(Bucket, RequesterId) of
        ok ->
            {true, ReqData, Ctx};
        {error, Reason} ->
            bucket_bouncer_response:api_error(Reason, ReqData, Ctx)
    end.
