bindkey -e

# options
setopt NOBEEP
setopt NOCORRECT
setopt AUTO_CD
setopt AUTO_PUSHD
setopt PUSHD_IGNORE_DUPS
setopt PUSHD_SILENT
setopt MENU_COMPLETE
setopt AUTO_LIST
setopt ALWAYS_TO_END
setopt COMPLETE_IN_WORD
setopt BANG_HIST
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_SAVE_NO_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_VERIFY
setopt EXTENDED_HISTORY

# misc alias
alias ls='ls --color=auto'
alias ll="ls -alG"
alias cp='cp -iv'
alias mv='mv -iv'
alias rm='rm -iv'
alias :q=exit
alias ..='cd ..'
alias grep='grep --color=auto'
alias ga='git add'
alias gb='git branch'
alias gba='git branch -a'
alias gbd='git branch -d'
alias gc='git commit -S -v'
alias gca='git commit -S -v --amend'
alias gco='git checkout'
alias gcb='git checkout -b'
alias gd='git diff'
alias gdc='git diff --cached'
alias gds='git diff --staged'
alias gs='git status -sb'

# plugins
ZPLUGINDIR=${ZDOTDIR:-~/.config/zsh}/plugins
if [[ ! -d $ZPLUGINDIR/zsh_unplugged ]]; then
  git clone --quiet https://github.com/mattmc3/zsh_unplugged $ZPLUGINDIR/zsh_unplugged
fi
source $ZPLUGINDIR/zsh_unplugged/zsh_unplugged.zsh
plugins=(
  sindresorhus/pure
  zsh-users/zsh-autosuggestions
  zsh-users/zsh-completions
  zdharma-continuum/fast-syntax-highlighting)
plugin-load $plugins

autoload -U colors; colors
autoload -U compinit; compinit
autoload -U promptinit; promptinit

prompt pure
zstyle :prompt:pure:git:stash show yes

typeset -U path
path=(~/.local/bin ~/.ghcup/bin $path)
export PATH

export TERM=xterm-256color

export EDITOR='vim'
export VISUAL='vim'

export HISTSIZE=10000
export SAVEHIST=10000
export HISTFILE="$HOME/.zsh_history"

# pinentry
export GPG_TTY=$(tty)

# emacs-eat
[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
  source "$EAT_SHELL_INTEGRATION_DIR/zsh"

