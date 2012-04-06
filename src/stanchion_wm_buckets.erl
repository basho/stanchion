%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

-module(stanchion_wm_buckets).

-export([init/1,
         service_available/2,
         allowed_methods/2,
         is_authorized/2,
         content_types_provided/2,
         post_is_create/2,
         create_path/2,
         content_types_accepted/2,
         accept_body/2,
         to_xml/2]).

-include("stanchion.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init(Config) ->
    %% Check if authentication is disabled and
    %% set that in the context.
    AuthBypass = proplists:get_value(auth_bypass, Config),
    {ok, #context{auth_bypass=AuthBypass}}.

-spec service_available(term(), term()) -> {true, term(), term()}.
service_available(RD, Ctx) ->
    stanchion_wm_utils:service_available(RD, Ctx).

%% @doc Get the list of methods this resource supports.
-spec allowed_methods(term(), term()) -> {[atom()], term(), term()}.
allowed_methods(RD, Ctx) ->
    {['GET', 'POST'], RD, Ctx}.

%% @doc Check that the request is from the admin user
is_authorized(RD, Ctx=#context{auth_bypass=AuthBypass}) ->
    AuthHeader = wrq:get_req_header("authorization", RD),
    case stanchion_wm_utils:parse_auth_header(AuthHeader, AuthBypass) of
        {ok, AuthMod, Args} ->
            case AuthMod:authenticate(RD, Args) of
                ok ->
                    %% Authentication succeeded
                    {true, RD, Ctx};
                {error, _Reason} ->
                    %% Authentication failed, deny access
                    stanchion_response:api_error(access_denied, RD, Ctx)
            end
    end.

-spec content_types_provided(term(), term()) ->
                                    {[{string(), atom()}],
                                     term(),
                                     term()}.
content_types_provided(RD, Ctx) ->
    %% @TODO Add JSON support
    {[{"application/xml", to_xml}], RD, Ctx}.

-spec post_is_create(term(), term()) -> {true, term(), term()}.
post_is_create(_RD, _Ctx) ->
    {true, _RD, _Ctx}.

%% @doc Set the path for the new bucket resource and set
%% the Location header to generate a 201 Created response.
%% -spec create_path(term(), term()) -> {string(), term(), term()}.
create_path(RD, Ctx) ->
    {wrq:disp_path(RD), RD, Ctx}.

-spec content_types_accepted(term(), term()) ->
                                    {[{string(), atom()}], term(), term()}.
content_types_accepted(RD, Ctx) ->
    {[{"application/json", accept_body}], RD, Ctx}.

%% @doc Create a bucket from a POST
-spec accept_body(term(), term()) ->
                         {true | {halt, pos_integer()},
                          term(),
                          term()}.
accept_body(RD, Ctx) ->
    %% @TODO POST is not the best method to use to
    %% handle creation of named buckets. Change the
    %% interface so that a PUT to /buckets/<bucketname>
    %% is the method for creating a specific bucket.
    Body = wrq:req_body(RD),
    %% @TODO Handle json decoding exceptions
    ParsedBody = mochijson2:decode(Body),
    FieldList = stanchion_wm_utils:json_to_proplist(ParsedBody),
    case stanchion_server:create_bucket(FieldList) of
        ok ->
            {true, RD, Ctx};
        {error, Reason} ->
            stanchion_response:api_error(Reason, RD, Ctx)
    end.

-spec to_xml(#wm_reqdata{}, term()) ->
                    {tuple(), #wm_reqdata{}, term()}.
to_xml(RD, Ctx) ->
    OwnerId = list_to_binary(wrq:get_qs_value("owner", "", RD)),
    case stanchion_utils:get_buckets(OwnerId) of
        {ok, BucketData} ->
            stanchion_response:list_buckets_response(BucketData,
                                                     RD,
                                                     Ctx);
        {error, Reason} ->
            stanchion_response:api_error(Reason, RD, Ctx)
    end.
