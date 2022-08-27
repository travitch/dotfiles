#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'

core_packages=(
    # System utilities
    emacs-nox
    fd-find
    starship
    tmux
    zsh
    htop
    powertop
    ripgrep
    exa
    skim
    tokei
    hunspell
    elinks
    mutt
    fuse-sshfs
    curl
    zip
    unzip
    lshw
    jq
    rdiff-backup
    openssh
    gnupg2
    docker-ce
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
    ninja-build
    cmake
    doxygen
    lua
    ctags
    golang
    texinfo
    musl-devel
    musl-gcc
    musl-clang
    # SCM
    git
    git-lfs
    tig
    mercurial
    # Python
    python3-ipython
    python3-jupyter-console
)

gui_packages=(
    # GUI
    i3
    i3lock
    dunst
    rofi
    pavucontrol
    graphviz
    playerctl
    obs-studio

    # Tex
    texlive
    texlive-xetex
    latexmk

    # Tools
    vim-X11
)

CARGO_PKGS=( du-dust
             bat
             flamegraph
             )


# Download URLs for various solvers
Z3_URL=https://github.com/Z3Prover/z3/releases/download/z3-4.8.7/z3-4.8.7-x64-ubuntu-16.04.zip
YICES_URL=https://yices.csl.sri.com/releases/2.6.1/yices-2.6.1-x86_64-pc-linux-gnu-static-gmp.tar.gz
CVC4_URL=https://github.com/CVC4/CVC4/releases/download/1.7/cvc4-1.7-x86_64-linux-opt

install_cargo_pkgs() {
    mkdir -p $HOME/.cargo/installed
    for PKG in "${CARGO_PKGS[@]}"
    do
        local GUARD=$HOME/.cargo/installed/$PKG
        if [ ! -f $GUARD ]
        then
            cargo install --force "${PKG}"
            touch $GUARD
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
        curl "${GHCUP_URL}" > ${GHCUP}
        chmod +x ${GHCUP}
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

RUSTUP=$HOME/.cargo/bin/rustup

install_rustup() {
    if [ ! -x "${RUSTUP}" ]
    then
        curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
    fi
}


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
        local tmpdir=$(mktemp -d)
        pushd "${tmpdir}"
        curl -L -o yices.tar.gz "${YICES_URL}"
        tar xf yices.tar.gz
        cp yices-*/bin/yices* "${HOME}/.local/bin/"
        popd
        rm -rf "${tmpdir}"
    fi
}

install_solvers() {
    # install_z3
    # install_yices
    # install_cvc4
    echo "solvers"
}

REMOVED_PKGS=( chromium-browser
               chromium-codecs-ffmpeg-extra
               chromium-browser-l10n
             )

remove_obsolete() {
    # sudo apt-mark auto "${REMOVED_PKGS[@]}"
    echo "Remove obsolete"
}

install_packages() {
    local pkgs=($@)
    # Sort the package names (xargs breaks the single line up into a column)
    local sorted=$(xargs -n1 <<< ${pkgs[@]} | sort -u)
    # local manually_installed=$(apt-mark showmanual | xargs -n1 | sort -u)
    # local removed=$(comm -1 -3 <(echo "$sorted") <(echo "${manually_installed}"))

    sudo dnf install $sorted
    # sudo apt-mark auto $(echo "${removed}") || echo "No unneeded packages"
}

bootstrap_core() {
    local pkgs=( ${core_packages[@]} )
    install_packages ${pkgs[@]}
}

bootstrap_gui() {
    local pkgs=( ${core_packages[@]} ${gui_packages[@]} )
    install_packages ${pkgs[@]}
    # install_snaps
}

basic_setup() {
    mkdir -p $HOME/.local/bin
}

install_common() {
    basic_setup
    # setup_haskell
    install_rustup
    install_cargo_pkgs
    # install_solvers
}