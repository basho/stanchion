%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

-module(bucket_bouncer_wm_buckets).

-export([init/1,
         service_available/2,
         forbidden/2,
         content_types_provided/2,
         malformed_request/2,
         to_xml/2,
         allowed_methods/2,
         content_types_accepted/2,
         accept_body/2,
         delete_resource/2]).

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

%% @doc Check to see if the user is
%%      authenticated. Normally with HTTP
%%      we'd use the `authorized` callback,
%%      but this is how S3 does things.
forbidden(RD, Ctx=#context{auth_bypass=AuthBypass}) ->
    AuthHeader = wrq:get_req_header("authorization", RD),
    case bucket_bouncer_wm_utils:parse_auth_header(AuthHeader, AuthBypass) of
        {ok, AuthMod, Args} ->
            case AuthMod:authenticate(RD, Args) of
                {ok, User} ->
                    %% Authentication succeeded
                    {false, RD, Ctx#context{user=User}};
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

%% @spec content_types_accepted(reqdata(), context()) ->
%%          {[{ContentType::string(), Acceptor::atom()}],
%%           reqdata(), context()}
content_types_accepted(RD, Ctx) ->
    case wrq:get_req_header("content-type", RD) of
        undefined ->
            {[{"application/octet-stream", accept_body}], RD, Ctx};
        CType ->
            {[{CType, accept_body}], RD, Ctx}
    end.


-spec to_xml(term(), term()) ->
    {iolist(), term(), term()}.
to_xml(RD, Ctx=#context{user=User}) ->
    BucketName = wrq:path_info(bucket, RD),
    Bucket = hd([B || B <- bucket_bouncer_utils:get_buckets(User), B#moss_bucket.name =:= BucketName]),
    MOSSBucket = bucket_bouncer_utils:to_bucket_name(objects, list_to_binary(Bucket#moss_bucket.name)),
    Prefix = list_to_binary(wrq:get_qs_value("prefix", "", RD)),
    case bucket_bouncer_utils:get_keys_and_objects(MOSSBucket, Prefix) of
        {ok, KeyObjPairs} ->
            bucket_bouncer_response:list_bucket_response(User,
                                                       Bucket,
                                                       KeyObjPairs,
                                                       RD,
                                                       Ctx);
        {error, Reason} ->
            bucket_bouncer_response:api_error(Reason, RD, Ctx)
    end.

%% TODO:
%% Add content_types_accepted when we add
%% in PUT and POST requests.
accept_body(ReqData, Ctx=#context{user=User}) ->
    case bucket_bouncer_utils:create_bucket(User#moss_user.key_id,
                                       wrq:path_info(bucket, ReqData)) of
        ok ->
            {{halt, 200}, ReqData, Ctx};
        ignore ->
            bucket_bouncer_response:api_error(bucket_already_exists, ReqData, Ctx);
        {error, Reason} ->
            bucket_bouncer_response:api_error(Reason, ReqData, Ctx)
    end.

%% @doc Callback for deleting a bucket.
-spec delete_resource(term(), term()) -> boolean().
delete_resource(ReqData, Ctx=#context{user=User}) ->
    BucketName = wrq:path_info(bucket, ReqData),
    case bucket_bouncer_utils:delete_bucket(User#moss_user.key_id,
                                       BucketName) of
        ok ->
            {true, ReqData, Ctx};
        {error, Reason} ->
            bucket_bouncer_response:api_error(Reason, ReqData, Ctx)
    end.
