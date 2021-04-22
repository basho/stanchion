FROM erlang:22.3.4.10 AS compile-image

ARG riak_host=127.0.0.1 \
    riak_pb_port=8087 \
    stanchion_port=8085

EXPOSE $stanchion_port

WORKDIR /usr/src/stanchion
COPY . /usr/src/stanchion

RUN sed -i \
    -e "s/@stanchion_port@/$stanchion_port/" \
    -e "s/@riak_ip@/$riak_host/" \
    -e "s/@riak_pb_port@/${riak_pb_port}/" \
    rel/docker/vars.config
RUN ./rebar3 as docker release

FROM debian:buster AS runtime-image

RUN apt-get update && apt-get -y install libssl1.1

COPY --from=compile-image /usr/src/stanchion/_build/docker/rel/stanchion /opt/stanchion

RUN mkdir -p /etc/stanchion /var/lib/stanchion /var/lib/stanchion/data
RUN mv /opt/stanchion/etc/stanchion.conf /etc/stanchion

COPY --from=compile-image /usr/src/stanchion/rel/docker/tini /tini
RUN chmod +x /tini

ENTRYPOINT ["/tini", "--"]
CMD ["/opt/stanchion/bin/stanchion", "foreground"]
