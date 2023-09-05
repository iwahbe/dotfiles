#!/usr/bin/env zsh

# Pre-append manually set paths to the path variable
path=($user_path $path)

alias aws-login='aws sso login --profile=dev-sandbox && eval $(aws-sso-creds export -p dev-sandbox)'

if exe_exists exa; then
    alias ls='exa -Fl'
else
    alias ls='ls -Fl'
fi

alias cl='clear; ls'

if exe_exists pulumi; then
    alias pu=pulumi
fi

if exe_exists terraform; then
    alias tf=terraform
fi

if exe_exists gmake; then
    alias make=gmake
fi

if exe_exists zoxide; then
   eval "$(zoxide init zsh --cmd c)"
fi

if exe_exists starship; then
    eval "$(starship init zsh)"
fi

if exe_exists atuin; then
    # Note: This generates the config file ($HOME/.config/atuin/config.toml) when
    # run. AFAIK, this behavior cannot be ignored. I have opened
    # https://github.com/atuinsh/atuin/issues/1180 to track.
    eval "$(atuin init --disable-up-arrow zsh)"
fi

# Terminal side configuration for libvterm.
#
# This needs to run after `starship init zsh` to work, since starship overwrites $PROMPT,
# and this works by appending to it.
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    # When we are inside Emacs, we don't want to open Emacs again, since it is hard to
    # exit the inner Emacs without quitting the terminal emulator or the outer Emacs
    # instance.
    #
    # To solve this, we redirect the two most common ways I open Emacs:
    #
    # Opening Emacs directly in the shell:
    #
    #	$ emacs file.txt
    #
    alias emacs='emacsclient --quiet' # Avoid recursive Emacs.
    # Opening Emacs via $EDITOR:
    #
    #	$ git commit
    #
    export EDITOR='emacsclient --quiet'
    local src="${EMACS_VTERM_PATH}etc/emacs-vterm-zsh.sh"
    if [[ -f "$src" ]]; then
        source "$src"
    else
        echo "Could not find vterm fish file to source: $src"
    fi
fi

local HIGHLIGHT="$HOME/.cache/zsh-syntax-highlighting"
if ! [[ -d "$HIGHLIGHT" ]]; then
    echo "Cloning zsh syntax highlighting into $HIGHLIGHT"
    mkdir -d "$HOME/.cache"
    git clone 'https://github.com/zsh-users/zsh-syntax-highlighting.git' "$HIGHLIGHT"
fi
source "$HIGHLIGHT/zsh-syntax-highlighting.zsh"
