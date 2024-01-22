set -l EMACS /usr/local/emacs/current/bin
set -l HOMEBREW $HOME/.homebrew/bin:/opt/homebrew/bin
set -l GHC $HOME/.ghcup/bin
set -l CABAL $HOME/.cabal/bin
set -l CARGO $HOME/.cargo/bin
set -l Z3 $HOME/Tools/Z3/current/bin
set -l CVC5 $HOME/Tools/CVC5/current/bin

set -gx EDITOR 'emacs -nw'
set -gx PATH $EMACS:$HOME/Scripts:$CARGO:$HOME/.local/bin:$Z3:$CVC5:$HOMEBREW:$GHC:$CABAL:/bin:/usr/local/bin:/usr/bin:/usr/sbin:/sbin
set -gx CLICOLOR 1

alias e 'emacs -nw'

set LOCAL_ENV $HOME/.config/local-environment.fish

if test -f $LOCAL_ENV
    source $LOCAL_ENV
end
