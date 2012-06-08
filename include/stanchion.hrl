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

-define(ACL, #acl_v2).
-define(USER_BUCKET, <<"moss.users">>).
-define(BUCKETS_BUCKET, <<"moss.buckets">>).
-define(FREE_BUCKET_MARKER, <<"0">>).
-define(MOSS_USER, #rcs_user_v2).
-define(RCS_USER, #rcs_user_v2).
-define(MD_ACL, <<"X-Moss-Acl">>).
