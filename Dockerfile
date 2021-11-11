FROM erlang:22 AS compile-image

EXPOSE 8085

WORKDIR /usr/src/stanchion
COPY . /usr/src/stanchion

# When running in a docker container, ideally we would want our app to
# be configurable via environment variables (option --env-file to
# docker run).  For that reason, We use a pared-down, cuttlefish-less
# rebar.config.  Configuration from environment now becomes possible,
# via rebar's own method of generating sys.config from
# /sys.config.src.
RUN make rel-docker

FROM debian:bullseye AS runtime-image

COPY --from=compile-image /usr/src/stanchion/rel/stanchion /opt/stanchion

CMD /opt/stanchion/bin/stanchion foreground
