#!/usr/bin/env bash
# Environment settings shared across multiple machines

# Select the most recent emacs
EMACS=/usr/local/emacs/current/bin
HOMEBREW=$HOME/.homebrew/bin:/opt/homebrew/bin

export PATH=$EMACS:$HOME/.local/bin:$HOME/.ghcup/bin:$HOME/.cargo/bin:$HOMEBREW:$HOME/.cabal/bin:/usr/local/bin:/usr/bin:/bin:$HOME/Scripts:$HOME/.emacs.d/language-servers/bin
export EDITOR='emacs -nw'
export CLICOLOR=1

if [[ ! -v SSH_AUTH_SOCK ]];
then

    export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
fi

LOCAL_ENV="$HOME/.config/local-environment.sh"
[[ -f "$LOCAL_ENV" ]] && source "$LOCAL_ENV"
