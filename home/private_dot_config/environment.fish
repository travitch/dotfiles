set -u EMACS /usr/local/emacs/current/bin
set -u HOMEBREW $HOME/.homebrew/bin:/opt/homebrew/bin
set -u GHC $HOME/.ghcup/bin
set -u CABAL $HOME/.cabal/bin
set -u CARGO $HOME/.cargo/bin
set -u Z3 $HOME/Tools/Z3/current/bin
set -u CVC5 $HOME/Tools/CVC5/current/bin
set -u DAFNY $HOME/Tools/Dafny/4.4.0

set -gx EDITOR 'emacs -nw'
set -gx PATH $EMACS:$HOME/Scripts:$CARGO:$HOME/.local/bin:$Z3:$CVC5:$HOMEBREW:$GHC:$CABAL:$DAFNY:/bin:/usr/local/bin:/usr/bin:/usr/sbin:/sbin
set -gx CLICOLOR 1
set -gx COLORTERM truecolor

alias e 'emacs -nw'

if type -q mise
    mise activate fish | source
end

set LOCAL_ENV $HOME/.config/local-environment.fish

if test -f $LOCAL_ENV
    source $LOCAL_ENV
end
