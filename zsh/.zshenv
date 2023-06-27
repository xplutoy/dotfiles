# yx
export YX_DOTDIR=$HOME/dotfiles/
export YX_ORGDIR=$HOME/personal/org/

# term
export TERM=xterm-256color

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
export PASSWORD_STORE_DIR=$HOME/privacy/password-store/

# emacs-eat
export EAT_SHELL_INTEGRATION_DIR=$HOME/.emacs.d/site-lisp/emacs-eat/integration/

# pinentry
export GPG_TTY=$(tty)
