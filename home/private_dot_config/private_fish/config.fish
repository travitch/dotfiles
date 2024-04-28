source $HOME/.config/environment.fish

if status is-interactive
    # Commands to run in interactive sessions can go here
    set -U fish_greeting

    if type -q starship
        starship init fish | source
    end

    # Disable the variables binding, which is Ctrl+V, which interferes with copy/paste
    fzf_configure_bindings --variables=
end
