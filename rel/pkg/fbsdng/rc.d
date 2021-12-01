#!/bin/sh

# $FreeBSD$
#
# PROVIDE: stanchion
# REQUIRE: LOGIN
# KEYWORD: shutdown

. /etc/rc.subr

name=stanchion
command=/usr/local/lib/stanchion/%ERTS_PATH%/bin/beam.smp
rcvar=stanchion_enable
start_cmd="/usr/local/bin/stanchion start"
stop_cmd="/usr/local/bin/stanchion stop"
pidfile="/var/run/stanchion/stanchion.pid"

load_rc_config $name
run_rc_command "$1"
