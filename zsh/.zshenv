# yx
export YX_DOCDIR=$HOME/dotdocs/
export YX_DOTDIR=$HOME/dotfiles/

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
export PASSWORD_STORE_DIR=$YX_DOCDIR/password-store/

# emacs-eat
export EAT_SHELL_INTEGRATION_DIR=$HOME/.emacs.d/site-lisp/emacs-eat/integration/

# pinentry
export GPG_TTY=$(tty)

# global
export GTAGSOBJDIRPREFIX=~/.cache/gtags/
[[ ! -d "$GTAGSOBJDIRPREFIX" ]] && mkdir -p "$GTAGSOBJDIRPREFIX"
export GTAGSCONF=/usr/share/gtags/gtags.conf
export GTAGSLABEL=native-pygments
