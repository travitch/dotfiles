source $HOME/.config/environment.fish

if status is-interactive
    # Commands to run in interactive sessions can go here
    set -U fish_greeting

    starship init fish | source

    # Disable the variables binding, which is Ctrl+V, which interferes with copy/paste
    fzf_configure_bindings --variables=
end
