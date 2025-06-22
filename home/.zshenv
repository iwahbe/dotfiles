#!/bin/zsh
typeset -T USER_PATH user_path

# exa_exists <executable>
function exe_exists {
    type "$1" > /dev/null;
    return $?;
}

# add_to_path <path>
function add_to_path {
    path=("$1" $path)
    user_path=("$1" $user_path)
}
export USER_PATH

if [[ -d "/usr/local/bin" ]]; then
   add_to_path "/usr/local/bin"
fi

if [[ -d "/opt/homebrew/bin" ]]; then
   add_to_path "/opt/homebrew/bin"
   alias brew="/opt/homebrew/bin/brew"
fi

if [[ -d "/usr/local/Homebrew/bin" ]]; then
   alias oldbrew="/usr/local/Homebrew/bin/brew"
fi

if [[ -d "$HOME/.local/bin" ]]; then
    add_to_path "$HOME/.local/bin"
fi

if [[ -f "$HOME/.cargo/env" ]]; then
   source "$HOME/.cargo/env"
fi

if exe_exists go; then
   export GOPATH="$HOME/go"
   export GOBIN="$GOPATH/bin"
   add_to_path "$GOBIN"
fi

if exe_exists dotnet; then
   add_to_path "$HOME/.dotnet/tools"
fi

if exe_exists emacs; then
    export EDITOR='emacs -nw'
fi

if [[ -d "$HOME/.pulumi/bin" ]]; then
    add_to_path "$HOME/.pulumi/bin"
    export PULUMI_SUPPRESS_COPILOT_LINK=true
fi

if exe_exists bun; then
    export BUN_INSTALL="$HOME/.bun"
    export PATH="$BUN_INSTALL/bin:$PATH"
fi

if [[ -d "/usr/local/texlive/2024basic/bin/universal-darwin" ]]; then
    add_to_path "/usr/local/texlive/2024basic/bin/universal-darwin"
fi

# bun completions
[ -s "$HOME/.bun/_bun" ] && source "$HOME/.bun/_bun"

# Haskell
[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env" # ghcup-env
