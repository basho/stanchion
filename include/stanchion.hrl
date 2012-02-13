-record(context, {auth_bypass :: boolean(),
                  bucket :: binary(),
                  owner_id :: all | string()}).

-record(moss_user_v1, {
          name :: string(),
          display_name :: string(),
          email :: string(),
          key_id :: string(),
          key_secret :: string(),
          canonical_id :: string()}).
-type moss_user() :: #moss_user_v1{}.

-define(USER_BUCKET, <<"moss.users">>).
-define(BUCKETS_BUCKET, <<"moss.buckets">>).
-define(FREE_BUCKET_MARKER, <<"0">>).
-define(MOSS_USER, #moss_user_v1).
