-record(context, {bucket :: binary(),
                  owner_id :: all | string()}).

-define(BUCKETS_BUCKET, <<"moss.buckets">>).
-define(FREE_BUCKET_MARKER, <<"0">>).
