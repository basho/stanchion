-record(context, {auth_bypass :: boolean(),
                  bucket :: binary(),
                  owner_id :: all | string()}).

-record(moss_user_v1, {
          name :: string(),
          display_name :: string(),
          email :: string(),
          key_id :: string(),
          key_secret :: string(),
          canonical_id :: string(),
          buckets=[] :: [moss_bucket()]}).

-record(rcs_user_v2, {
          name :: string(),
          display_name :: string(),
          email :: string(),
          key_id :: string(),
          key_secret :: string(),
          canonical_id :: string(),
          buckets=[] :: [moss_bucket()],
          status=enabled :: enabled | disabled}).
-type moss_user() :: #rcs_user_v2{} | #moss_user_v1{}.
-type rcs_user() :: #rcs_user_v2{} | #moss_user_v1{}.

-record(moss_bucket_v1, {
          name :: string(),
          last_action :: created | deleted,
          creation_date :: string(),
          modification_time :: erlang:timestamp(),
          acl :: acl()}).

-type moss_bucket() :: #moss_bucket_v1{}.

-type acl_perm() :: 'READ' | 'WRITE' | 'READ_ACP' | 'WRITE_ACP' | 'FULL_CONTROL'.
-type acl_perms() :: [acl_perm()].
-type acl_grant() :: {{string(), string()}, acl_perms()} | {atom(), acl_perms()}.
-type acl_owner() :: {string(), string()} | {string(), string(), string()}.
-record(acl_v1, {owner={"", ""} :: acl_owner(),
                 grants=[] :: [acl_grant()],
                 creation_time=now() :: erlang:timestamp()}).
-record(acl_v2, {owner={"", "", ""} :: acl_owner(),
                 grants=[] :: [acl_grant()],
                 creation_time=now() :: erlang:timestamp()}).
-type acl1() :: #acl_v1{}.
-type acl2() :: #acl_v2{}.
-type acl() :: acl1() | acl2().


% S3 Actions
-type object_action() :: 's3:GetObject'       | 's3:GetObjectVersion'
                       | 's3:GetObjectAcl'    | 's3:GetObjectVersionAcl'
                       | 's3:PutObject'       | 's3:PutObjectAcl'
                       | 's3:PutObjectVersionAcl'
                       | 's3:DeleteObject'    | 's3:DeleteObjectVersion'
                       | 's3:ListMultipartUploadParts' %to be supported
                       | 's3:AbortMultipartUpload'     %to be supported
                       %| 's3:GetObjectTorrent'         we never do this
                       %| 's3:GetObjectVersionTorrent'  we never do this
                       | 's3:RestoreObject'.

-define(SUPPORTED_OBJECT_ACTION,
        [ 's3:GetObject', 's3:GetObjectAcl', 's3:PutObject', 's3:PutObjectAcl',
          's3:DeleteObject' ]).

-type bucket_action() :: 's3:CreateBucket'
                       | 's3:DeleteBucket'
                       | 's3:ListBucket'
                       | 's3:ListBucketVersions'
                       | 's3:ListAllMyBuckets'
                       | 's3:ListBucketMultipartUploads'
                       | 's3:GetBucketAcl' | 's3:PutBucketAcl'
                       | 's3:GetBucketVersioning' | 's3:PutBucketVersioning'
                       | 's3:GetBucketRequestPayment' | 's3:PutBucketRequestPayment'
                       | 's3:GetBucketLocation'
                       | 's3:GetBucketPolicy' | 's3:DeleteBucketPolicy' | 's3:PutBucketPolicy'
                       | 's3:GetBucketNotification' | 's3:PutBucketNotification'
                       | 's3:GetBucketLogging' | 's3:PutBucketLogging'
                       | 's3:GetBucketWebsite' | 's3:PutBucketWebsite' | 's3:DeleteBucketWebsite'
                       | 's3:GetLifecycleConfiguration' | 's3:PutLifecycleConfiguration'.

-define(SUPPORTED_BUCKET_ACTION,
        [ 's3:CreateBucket', 's3:DeleteBucket', 's3:ListBucket', 's3:ListAllMyBuckets',
          's3:GetBucketAcl', 's3:PutBucketAcl',
          's3:GetBucketPolicy', 's3:DeleteBucketPolicy', 's3:PutBucketPolicy']).

% one of string, numeric, date&time, boolean, IP address, ARN and existence of condition keys
-type string_condition_type() :: 'StringEquals' | streq            | 'StringNotEquals' | strneq
                               | 'StringEqualsIgnoreCase' | streqi | 'StringNotEqualsIgnoreCase' | streqni
                               | 'StringLike' | strl               | 'StringNotLike' | strnl.

-type numeric_condition_type() :: 'NumericEquals' | numeq      | 'NumericNotEquals' | numneq
                                | 'NumericLessThan'  | numlt   | 'NumericLessThanEquals' | numlteq
                                | 'NumericGreaterThan' | numgt | 'NumericGreaterThanEquals' | numgteq.


-type date_condition_type() :: 'DateEquals'         | dateeq
                             | 'DateNotEquals'      | dateneq
                             | 'DateLessThan'       | datelt
                             | 'DateLessThanEquals' | datelteq
                             | 'DateGreaterThan'    | dategt
                             | 'DateGreaterThanEquals' | dategteq.

-type ip_addr_condition_type() :: 'IpAddress' | 'NotIpAddress'.

-type condition_pair() :: {date_condition_type(), [{'aws:CurrentTime', binary()}]} % or now()?
                        | {numeric_condition_type(), [{'aws:EpochTime', non_neg_integer()}]}
                        %| {boolean(), 'MultiFactorAuthAge'}
                        | {boolean(), 'aws:SecureTransport'}
                        | {ip_addr_condition_type(), [{'aws:SourceIp', {IP::inet:ip_addr(), inet:ip_addr()}}]}
                        | {string_condition_type(),  [{'aws:UserAgent', binary()}]}
                        | {string_condition_type(),  [{'aws:Referer', binary()}]}.

-record(arn_v1, {
          provider = aws :: aws,
          service  = s3  :: s3,
          region         :: binary(),
          id             :: binary(),
          path           :: binary()
         }).

-type arn() :: #arn_v1{}.

% http://docs.amazonwebservices.com/AmazonS3/latest/dev/BasicStructure.html
% built from usecases:
% http://docs.amazonwebservices.com/AmazonS3/latest/dev/AccessPolicyLanguage_UseCases_s3_a.html
-record(statement, {
          sid = undefined :: binary(), % had better use uuid: should be UNIQUE
          effect = deny :: allow | deny,
          % TODO: account ID, with or without hyphen 1234-5678-9012, what for us?
          principal  = [] :: [{binary(), [binary()]}],
          action     = [] :: [ object_action() | bucket_action() ] | '*',
          not_action = [] :: [ object_action() | bucket_action() ] | '*',
          resource =   [] :: [ arn() ] | '*',
          condition_block = [] :: [ condition_pair() ]
         }).

-record(policy_v1, {
          version = <<"2008-10-17">> :: binary(),  % no other value is allowed than default
          id = undefined :: binary(),  % had better use uuid: should be UNIQUE
          statement = [] :: #statement{}
         }).
-type policy1() :: #policy_v1{}.
-type policy() :: policy1().

-record(access_v1, {
          action :: object_action() | bucket_action(),
          id :: binary(),
          bucket :: binary(),
          key = <<>> :: binary(),
          req %:: #wm_reqdata{} % request of webmachine
         }).

-type principal() :: '*'
                   | [{canonical_id, string()}|{aws, '*'}].

-type access() :: #access_v1{}.

-define(ACL, #acl_v2).
-define(POLICY, #policy_v1).
-define(USER_BUCKET, <<"moss.users">>).
-define(BUCKETS_BUCKET, <<"moss.buckets">>).
-define(FREE_BUCKET_MARKER, <<"0">>).
-define(MOSS_USER, #rcs_user_v2).
-define(RCS_USER, #rcs_user_v2).
-define(MD_ACL, <<"X-Moss-Acl">>).
-define(MD_POLICY, <<"X-Moss-Policy">>).
