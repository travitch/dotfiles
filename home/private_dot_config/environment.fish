set -u EMACS /usr/local/emacs/current/bin
set -u HOMEBREW $HOME/.homebrew/bin:/opt/homebrew/bin:/home/linuxbrew/.linuxbrew/bin
set -u CARGO $HOME/.cargo/bin
set -u CVC5 $HOME/Tools/CVC5/current/bin
set -u ELAN $HOME/.elan/bin

set -gx EDITOR 'emacs -nw'
set -gx PATH $EMACS:$HOME/Scripts:$CARGO:$ELAN:$HOME/.local/bin:$CVC5:$HOMEBREW:/bin:/usr/local/bin:/usr/bin:/usr/sbin:/sbin
set -gx CLICOLOR 1
set -gx COLORTERM truecolor

alias e 'emacs -nw'

# On Linux, set the SSH agent socket
if test -d /etc/xdg
    set -gx SSH_AUTH_SOCK $XDG_RUNTIME_DIR/ssh-agent.socket
end

set LOCAL_ENV $HOME/.config/local-environment.fish

if test -f $LOCAL_ENV
    source $LOCAL_ENV
end
