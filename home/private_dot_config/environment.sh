#!/usr/bin/env bash
# Environment settings shared across multiple machines

EMACS=/usr/local/emacs/current/bin
export PATH=$EMACS:$HOME/.local/bin:$HOME/.ghcup/bin:$HOME/.cargo/bin:$HOME/.cabal/bin:$HOME/.homebrew/bin:/usr/local/bin:/usr/bin:/bin
export EDITOR='emacs -nw'
export CLICOLOR=1

# Configuration for emacs LSP that causes it to use a more efficient serialization mechanism
#
# This must be set when building lsp-mode
export LSP_USE_PLISTS=true

if [[ ! -v SSH_AUTH_SOCK ]];
then

    export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
fi

LOCAL_ENV="$HOME/.config/local-environment.sh"
[[ -f "$LOCAL_ENV" ]] && source "$LOCAL_ENV"
