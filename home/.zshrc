#!/usr/bin/env zsh

# Pre-append manually set paths to the path variable
path=($user_path $path)

_zsh_cache="$HOME/.cache/zsh"
if ! [ -d "$_zsh_cache" ]; then
    echo "Initializing zsh cache"
    mkdir -p "$_zsh_cache"
fi

_zsh_comp_dump="$_zsh_cache/zcompdump-$HOST"

# Registered completions
#
# Each entry should be a string of the form:
#
#	<cmd>:<completion generation command>
#
# Running <completion generation command> must emit the completion file for <cmd> to
# stdout.
completions_sources=(
    "pulumi:pulumi gen-completion zsh"
    "rustup:rustup completions zsh"
    "cargo:rustup completions zsh cargo"
    "rg:curl \"https://raw.githubusercontent.com/BurntSushi/ripgrep/master/complete/_rg\""
    "exa:curl \"https://raw.githubusercontent.com/ogham/exa/master/completions/zsh/_exa\""
    "go:curl \"https://raw.githubusercontent.com/zsh-users/zsh-completions/master/src/_golang\""
)

# The directory where completions are stored.
_completions_dir="$_zsh_cache/completions"

# The path where <cmd> should store it's completions file.
_completions_file () {
    echo "$_completions_dir/_$1"
}

# Run $1 on each <command> <generate> pair registered.
_completions_foreach () {
    for completion in $completions_sources; do
        command="$(echo "$completion" | cut -d':' -f 1)"
        gen="$(echo "$completion"     | cut -d':' -f 2-)"
        "$1" "$command" "$gen"
    done
}

_completions_regen () {
    if exe_exists "$1"; then
        echo "$1:@$2 @> $(_completions_file "$1")"
        eval "$2" > "$(_completions_file "$1")"
    else
        echo "$command@not found"
    fi
}

completions_regenerate () {
    rm -rf "$_completions_dir"
    rm "$_zsh_comp_dump"
    mkdir -p "$_completions_dir"
    echo "Rebuilding completions"
    _completions_foreach _completions_regen | column -t -s "@"
    autoload -Uz compinit && compinit -i -d "$_zsh_comp_dump"
}

_completions_ensure () {
    if exe_exists "$1" && ! [ -f "$(_completions_file "$1")" ]; then
       echo "$1:@$2 @> $(_completions_file "$1")"
       eval "$2" > "$(_completions_file "$1")"
    fi
}

completions_ensure () {
    mkdir -p "$_completions_dir" # ensure directory exists
    _completions_foreach _completions_ensure | column -t -s "@"
    autoload -Uz compinit && compinit -i -d "$_zsh_comp_dump"
}

fpath=($HOME/.cache/zsh/completions $fpath)
completions_ensure

# aws-login is responsible for dumping valid, non-production AWS credentials into the environment.
#
# Previously, I combined the aws CLI with https://github.com/jaxxstorm/aws-sso-creds to
# get credentials:
#
#     alias aws-login='aws sso login --profile=dev-sandbox && eval $(aws-sso-creds export -p dev-sandbox)'
#
# With the introduction of `pulumi env`, we can make this simpler:
alias aws-login='eval $(pulumi env open -f shell pulumi/dev-sandbox)'

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

# Setup syntax highlighting for zsh.
local highlighting="$_zsh_cache/zsh-syntax-highlighting"
if ! [[ -d "$highlighting" ]]; then
    echo "Cloning zsh syntax highlighting into $highlighting"
    git clone 'https://github.com/zsh-users/zsh-syntax-highlighting.git' "$highlighting"
fi
source "$highlighting/zsh-syntax-highlighting.zsh"

# bun completions
[ -s "$HOME/.bun/_bun" ] && source "$HOME/.bun/_bun"
