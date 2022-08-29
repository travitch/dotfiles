#!/usr/bin/env bash
#
# Set up a machine as much as possible without root
#
# Install cargo packages and anything else that can be done unprivileged

set -euo pipefail
IFS=$'\n\t'

source "${HOME}/Scripts/bootstrap.sh"
install_common
