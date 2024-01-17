set -gx EMACS /usr/local/emacs/current/bin
set -gx HOMEBREW $HOME/.homebrew/bin:/opt/homebrew/bin
set -gx GHC $HOME/.ghcup/bin
set -gx CABAL $HOME/.cabal/bin
set -gx CARGO $HOME/.cargo/bin
set -gx Z3 $HOME/Tools/Z3/current/bin
set -gx CVC5 $HOME/Tools/CVC5/current/bin

set -gx EDITOR 'emacs -nw'
set -gx PATH $EMACS:$HOME/Scripts:$CARGO:$HOME/.local/bin:$Z3:$CVC5:$HOMEBREW:$GHC:$CABAL:/bin:/usr/local/bin:/usr/bin:/usr/sbin:/sbin
set -gx CLICOLOR 1

alias e 'emacs -nw'

set LOCAL_ENV $HOME/.config/local-environment.fish

if test -f $LOCAL_ENV
    source $LOCAL_ENV
end
