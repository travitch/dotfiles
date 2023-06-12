#!/bin/bash
#
# This script sets up a connection to a remote server, tunnels a port, and
# starts a webserver on the same port that serves HTML from the given directory:
#
# remote-serve.sh $HOST $REMOTE_PATH
#
# This script is intended to enable remote access to generated documentation or
# HTML logs without copying.

set -euf -o pipefail

if [ $# -ne 2 ];
then
    echo "Usage: remote-serve.sh HOST REMOTE_PATH"
    exit 1
fi

# The port to open on the remote server and to tunnel to this host
#
# 0xd0c = 3340
HTTP_PORT=3340

# The host to connect to
REMOTE_HOST="$1"

# The remote path to serve up
REMOTE_PATH="$2"

# Open tunnel the remote port to localhost; note that we need to pass -N (run no
# command) so that we can run this tunnel connection in the background
ssh -N -L ${HTTP_PORT}:localhost:${HTTP_PORT} "${REMOTE_HOST}" &

# Note that the variable in the SSH command expands on this host, with the value
# in this script, as intended.  We pass -t to force TTY allocation so that, when
# the connection dies, the remote command sees its stdin return EOF and actually
# shuts down
ssh -x -t "${REMOTE_HOST}" "cd ${REMOTE_PATH} ; python3 -m http.server ${HTTP_PORT}"

# Clean up the tunnel job
trap 'kill $(jobs -p)' EXIT
