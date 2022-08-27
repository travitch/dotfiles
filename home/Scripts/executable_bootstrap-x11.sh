#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'

source "${HOME}/Scripts/bootstrap.sh"
bootstrap_gui
install_common
remove_obsolete
