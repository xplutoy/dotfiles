# yx
export YX_DOTDIR=$HOME/dotfiles/
export YX_ORGDIR=$HOME/personal/org/

# path
typeset -U path
path=(~/.local/bin $path)
export PATH

# hist
export HISTFILE="$ZDOTDIR/.zsh/.zsh_history"
export HISTSIZE=10000
export SAVEHIST=10000

# editor
export EDITOR='nvim'
export VISUAL='nvim'

# pass
export PASSWORD_STORE_DIR=$HOME/personal/password-store/
# scdv
export STARDICT_DATA_DIR="$HOME/.local/share/stardict/dic/"
