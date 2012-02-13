%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% -------------------------------------------------------------------

-module(stanchion_response).

-export([api_error/3,
         respond/4,
         error_response/5,
         list_bucket_response/5,
         list_buckets_response/3]).
-define(xml_prolog, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>").
-include("stanchion.hrl").

error_message(invalid_access_key_id) ->
    "The AWS Access Key Id you provided does not exist in our records.";
error_message(access_denied) ->
    "Access Denied";
error_message(bucket_not_empty) ->
    "The bucket you tried to delete is not empty.";
error_message(bucket_already_exists) ->
    "The requested bucket name is not available. The bucket namespace is shared by all users of the system. Please select a different name and try again.";
error_message(user_already_exists) ->
    "The specified email address has already been registered. Email addresses must be unique among all users of the system. Please try again with a different email address.";
error_message(entity_too_large) ->
    "Your proposed upload exceeds the maximum allowed object size.";
error_message(no_such_bucket) ->
    "The specified bucket does not exist.";
error_message({riak_connect_failed, Reason}) ->
    io_lib:format("Unable to establish connection to Riak. Reason: ~p", [Reason]).


error_code(invalid_access_key_id) -> "InvalidAccessKeyId";
error_code(access_denied) -> "AccessDenied";
error_code(bucket_not_empty) -> "BucketNotEmpty";
error_code(bucket_already_exists) -> "BucketAlreadyExists";
error_code(user_already_exists) -> "UserAlreadyExists";
error_code(entity_too_large) -> "EntityTooLarge";
error_code(no_such_bucket) -> "NoSuchBucket";
error_code({riak_connect_failed, _}) -> "RiakConnectFailed".


status_code(access_denied) ->  403;
status_code(bucket_not_empty) ->  409;
status_code(bucket_already_exists) -> 409;
status_code(user_already_exists) -> 409;
status_code(entity_too_large) -> 400;
status_code(invalid_access_key_id) -> 403;
status_code(no_such_bucket) -> 404;
status_code({riak_connect_failed, _}) -> 503.


respond(StatusCode, Body, ReqData, Ctx) ->
    {{halt, StatusCode}, wrq:set_resp_body(Body, ReqData), Ctx}.

api_error(Error, ReqData, Ctx) when is_atom(Error) ->
    error_response(status_code(Error), error_code(Error), error_message(Error),
                   ReqData, Ctx).

error_response(StatusCode, Code, Message, RD, Ctx) ->
    XmlDoc = [{'Error', [{'Code', [Code]},
                        {'Message', [Message]},
                        {'Resource', [wrq:path(RD)]},
                        {'RequestId', [""]}]}],
    respond(StatusCode, export_xml(XmlDoc), RD, Ctx).


list_buckets_response(BucketData, RD, Ctx) ->
    BucketsDoc = [{'Bucket',
                   [{'Name', [binary_to_list(Bucket)]},
                    {'Owner', [binary_to_list(Owner)]}]}
                  || {Bucket, Owner} <- BucketData],
    Contents = [{'Buckets', BucketsDoc}],
    XmlDoc = [{'ListBucketsResult',  Contents}],
    respond(200, export_xml(XmlDoc), RD, Ctx).

list_bucket_response(User, _Bucket, KeyObjPairs, RD, Ctx) ->
    Contents = [begin
                    KeyString = binary_to_list(Key),
                    LastModified = stanchion_wm_utils:iso_8601_datetime(),
                    case ObjResp of
                        {ok, Obj} ->
                            Manifest = binary_to_term(riakc_obj:get_value(Obj)),
                            case stanchion_lfs_utils:is_active(Manifest) of
                                true ->
                                    Size = integer_to_list(
                                             stanchion_lfs_utils:content_length(Manifest)),
                                    ETag = "\"" ++ stanchion_utils:binary_to_hexlist(
                                                     stanchion_lfs_utils:content_md5(Manifest))
                                        ++ "\"",
                                    {'Contents', [{'Key', [KeyString]},
                                                  {'Size', [Size]},
                                                  {'LastModified', [LastModified]},
                                                  {'ETag', [ETag]},
                                                  {'Owner', [User]}]};
                                false ->
                                    undefined
                            end;
                        {error, Reason} ->
                            lager:warning("Unable to fetch object for ~p. Reason: ~p",
                                          [Key, Reason]),
                            undefined
                    end
                end
                || {Key, ObjResp} <- KeyObjPairs],
    XmlDoc = [{'ListBucketResult',
                   lists:filter(fun(E) -> E /= undefined end,
                                Contents)}],
    respond(200, export_xml(XmlDoc), RD, Ctx).

export_xml(XmlDoc) ->
    unicode:characters_to_binary(
      xmerl:export_simple(XmlDoc, xmerl_xml, [{prolog, ?xml_prolog}])).
