#!/bin/zsh
if [[ "$INSIDE_EMACS" = vterm ]]; then
    src="$VTERM_DATA"
    if [[ -f "$src" ]]; then
        source "$src"
    else
        echo "Could not find vterm fish file to source: $src"
    fi
fi

# Pre-append manually set paths to the path variable
path=($user_path $path)

alias aws-login='aws sso login --profile=dev-sandbox && eval $(aws-sso-creds export -p dev-sandbox)'

if exe_exists exa; then
    alias ls='exa -Fl'
else
    alias ls='ls -Fl'
fi

alias cl='clear; ls'

if exe_exists gmake; then
    alias make=gmake
fi

if exe_exists zoxide; then
   eval "$(zoxide init zsh --cmd c)"
fi

if exe_exists starship; then
    eval "$(starship init zsh)"
fi

local HIGHLIGHT="$HOME/.cache/zsh-syntax-highlighting"
if ! [[ -d "$HIGHLIGHT" ]]; then
    echo "Cloning zsh syntax highlighting into $HIGHLIGHT"
    mkdir -d "$HOME/.cache"
    git clone 'https://github.com/zsh-users/zsh-syntax-highlighting.git' "$HIGHLIGHT"
fi
source $HIGHLIGHT/zsh-syntax-highlighting.zsh
