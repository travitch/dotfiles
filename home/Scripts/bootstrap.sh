#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'

core_packages=(
    # System utilities
    tmux
    zsh
    fish
    htop
    powertop
    hunspell
    curl
    zip
    unzip
    lshw
    jq
    rdiff-backup
    openssh
    gnupg2
    fzf

    # Dev things
    ShellCheck
    clang
    flex
    bison
    vim
    valgrind
    strace
    gperf
    gdb
    lldb
    autoconf
    automake
    cmake
    lua
    ctags
    texinfo

    # SCM
    git
    git-lfs
    tig
    mercurial
)

gui_packages=(
    # GUI
    i3
    i3lock
    i3status-rust
    dunst
    rofi
    pavucontrol
    graphviz
    doxygen
    playerctl
    obs-studio
    alacritty
    code

    # Tex
    texlive
    texlive-xetex
    latexmk

    # Tools
    vim-X11
    xdotool

    # Python
    python3-ipython
    python3-jupyter-console
)

ELAN=$HOME/.elan/bin/elan

install_elan() {
  if [ ! -x "${ELAN}" ]
  then
      curl https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh -sSf | sh -s -- --no-modify-path -y
  fi
}

RUSTUP=$HOME/.cargo/bin/rustup

install_rustup() {
    if [ ! -x "${RUSTUP}" ]
    then
        curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- --no-modify-path -y
        rustup component add rust-analyzer
    fi
}


install_mise_pkgs() {
    mise use -g go@1.22

    mise plugin install -y chezmoi
    mise use -g chezmoi

    mise plugin install cmake https://github.com/asdf-community/asdf-cmake.git
    mise use -g cmake

    mise plugin install -y fzf
    mise use -g fzf

    mise plugin install -y jq
    mise use -g jq

    mise plugin install -y maven
    mise use -g maven

    mise plugin install -y pandoc
    mise use -g pandoc

    mise plugin install -y yq
    mise use -g yq

    mise plugin install -y zola
    mise use -g zola

    mise install java@openjdk-17
    mise install java@openjdk-21
    mise use -g java@openjdk-21
}

CARGO_PKGS=( du-dust
             bat
             flamegraph
             ripgrep
             fd-find
             starship
             lsd
             tailspin
             skim
             tokei
             jql
             git-delta
             hexyl
             difftastic
             jless
             zellij
             spacer
             swordfish-rs
             bottom
             jaq
             procs
             tagref
             sd
             rust-parallel
             ouch
             mdcat
             hyperfine
             xh
             comrak
             television
             silicon
             )


install_cargo_pkgs() {
    . "$HOME/.cargo/env"
    mkdir -p "$HOME/.cargo/installed"
    for PKG in "${CARGO_PKGS[@]}"
    do
        local GUARD=$HOME/.cargo/installed/$PKG
        if [ ! -f "$GUARD" ]
        then
            cargo install --locked --force "${PKG}"
            touch "$GUARD"
        fi
    done
}

# The versions of GHC to install (via ghcup)
GHCS=( 8.4.4
       8.6.5
       8.8.3
       8.10.1
    )

GHCUP_URL=https://raw.githubusercontent.com/haskell/ghcup/master/ghcup
GHCUP=$HOME/.local/bin/ghcup

install_ghcup() {
    if [ ! -f "${GHCUP}" ]
    then
        curl "${GHCUP_URL}" > "${GHCUP}"
        chmod +x "${GHCUP}"
    fi
}

install_ghcs() {
    for GHCVER in "${GHCS[@]}"
    do
        if [ ! -f $HOME/.ghcup/bin/ghc-$GHCVER ]
        then
            ghcup install "${GHCVER}"
        fi
    done
    if [ ! -f $HOME/.ghcup/bin/cabal ]
    then
        ghcup install-cabal
    fi
}

setup_haskell() {
    install_ghcup
    install_ghcs
}



# Download URLs for various solvers
Z3_URL=https://github.com/Z3Prover/z3/releases/download/z3-4.8.7/z3-4.8.7-x64-ubuntu-16.04.zip
YICES_URL=https://yices.csl.sri.com/releases/2.6.1/yices-2.6.1-x86_64-pc-linux-gnu-static-gmp.tar.gz
CVC4_URL=https://github.com/CVC4/CVC4/releases/download/1.7/cvc4-1.7-x86_64-linux-opt

Z3=$HOME/.local/bin/z3
install_z3() {
    if [ ! -x "${Z3}" ]
    then
        local tmpdir=$(mktemp -d)
        pushd "${tmpdir}"
        # We have to use -L to get a github release tarball, which is actually a redirect
        curl -L -o z3.zip "${Z3_URL}"
        unzip z3.zip
        cp z3-*/bin/z3 "${Z3}"
        popd
        rm -rf "${tmpdir}"
    fi
}

CVC4=$HOME/.local/bin/cvc4
install_cvc4() {
    if [ ! -x "${CVC4}" ]
    then
        curl -L -o "${CVC4}" "${CVC4_URL}"
        chmod +x "${CVC4}"
    fi
}

YICES="$HOME/.local/bin/yices"
install_yices() {
    if [ ! -x "${YICES}" ]
    then
        local tmpdir
        tmpdir=$(mktemp -d)
        pushd "${tmpdir}"
        curl -L -o yices.tar.gz "${YICES_URL}"
        tar xf yices.tar.gz
        cp yices-*/bin/yices* "${HOME}/.local/bin/"
        popd
        rm -rf "${tmpdir}"
    fi
}

LANGUAGE_SERVER_ROOT=$HOME/.emacs.d/language-servers

install_bash_language_server() {
    if [ ! -d "${LANGUAGE_SERVER_ROOT}/bash-language-server" ]
    then
        pushd "${LANGUAGE_SERVER_ROOT}"
        git clone git@github.com:bash-lsp/bash-language-server.git
        cd bash-language-server
        # Haven't figured this out yet - it doesn't seem amenable to local installation
        npm i bash-language-server
        popd
    fi
}

JAVA_LS_URL="https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.43.0/jdt-language-server-1.43.0-202412191447.tar.gz"

install_java_language_server() {
    if [ ! -d "${LANGUAGE_SERVER_ROOT}/java-language-server" ]
    then
        mkdir -p "${LANGUAGE_SERVER_ROOT}/java-language-server"
        wget $JAVA_LS_URL --output-document=/tmp/jdtls.tar.gz
        tar xf /tmp/jdtls.tar.gz -C "${LANGUAGE_SERVER_ROOT}/java-language-server"
    fi
}

JAVA_DEBUG_ADAPTER_URL=https://github.com/microsoft/java-debug/archive/refs/tags/0.52.0.tar.gz

install_java_debug_adapter() {
    if [ ! -d "${LANGUAGE_SERVER_ROOT}/java-debug-adapter" ]
    then
        mkdir -p "${LANGUAGE_SERVER_ROOT}/java-debug-adapter"
        wget $JAVA_DEBUG_ADAPTER_URL --output-document=/tmp/java-debug-adapter.tar.gz
        tar xf /tmp/java-debug-adapter.tar.gz -C "${LANGUAGE_SERVER_ROOT}/java-debug-adapter" --strip-components=1
        pushd "${LANGUAGE_SERVER_ROOT}/java-debug-adapter"
        ./mvnw clean install
        popd
    fi
}

install_packages() {
    local pkgs=($@)
    # Sort the package names (xargs breaks the single line up into a column)
    local sorted=$(xargs -n1 <<< ${pkgs[@]} | sort -u)
    # local manually_installed=$(apt-mark showmanual | xargs -n1 | sort -u)
    # local removed=$(comm -1 -3 <(echo "$sorted") <(echo "${manually_installed}"))

    sudo dnf install --assumeyes $sorted
    # sudo apt-mark auto $(echo "${removed}") || echo "No unneeded packages"
}

bootstrap_core() {
    local pkgs=( ${core_packages[@]} )
    install_packages ${pkgs[@]}
}

add_fedora_repositories() {
    # Set up COPR to pull in some extra packages *before* installing GUI packages
    sudo dnf copr enable atim/i3status-rust -y

    # Add the upstream VSCode repo
    if [ ! -f /etc/yum.repos.d/vscode.repo ]
    then
        sudo rpm --import https://packages.microsoft.com/keys/microsoft.asc
        sudo sh -c 'echo -e "[code]\nname=Visual Studio Code\nbaseurl=https://packages.microsoft.com/yumrepos/vscode\nenabled=1\ngpgcheck=1\ngpgkey=https://packages.microsoft.com/keys/microsoft.asc" > /etc/yum.repos.d/vscode.repo'
    fi
}

bootstrap_gui() {
    add_fedora_repositories

    local pkgs=( ${core_packages[@]} ${gui_packages[@]} )
    install_packages ${pkgs[@]}
}

basic_setup() {
    mkdir -p $HOME/.local/bin
}

install_common() {
    basic_setup
    install_rustup
    install_elan

    # This needs cargo to be installed, but we also need the mise packages to be installed
    # (specifically, cmake) before the rest of the cargo packages can be installed
    if [[ ! -x $HOME/.cargo/bin/mise ]]
    then
        cargo install mise
    fi

    install_mise_pkgs

    install_cargo_pkgs

    install_java_language_server
    install_java_debug_adapter
}
