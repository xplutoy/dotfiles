bindkey -e

autoload -U colors; colors
autoload -U compinit
autoload -U promptinit; promptinit

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

# misc alias
alias cp='cp -iv'
alias mv='mv -iv'
alias rm='rm -iv'
alias :q=exit
alias ..='cd ..'
alias ls='exa --icons'
alias la='exa --icons -a'
alias ll='exa -l --icons -x -F --git-ignore --group-directories-first'
alias lt2='exa -T -L 2 --git-ignore'
alias grep='grep --color=auto'
alias ec='TERM=xterm-256color emacsclient -s server -t "$@"'
alias ecx='emacsclient -s server -n -c "$@"'
alias ga='git add'
alias gaa='git add -A'
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
alias gstl='git stash list'
alias gsta='git stash push'
alias gstd='git stash drop'
alias gstp='git stash pop'
alias gstc='git stash clear'

case "$OSTYPE" in
  darwin*)
    alias edge="open -a Microsoft\ Edge $1"
    ;;
  linux*)
    ;;
esac

# alias navi='navi --tldr'
alias r='radian'

# plugins
ZPLUGINDIR=${ZDOTDIR:-~/.config/zsh}/plugins
source $HOME/.zsh/plugins/zsh_unplugged_yx/zsh_unplugged.zsh
plugins=(
  sindresorhus/pure
  zsh-users/zsh-autosuggestions
  zsh-users/zsh-completions
  zdharma-continuum/fast-syntax-highlighting

  xplutoy/zsh-auto-venv-yx
  )
plugin-load $plugins

compinit
prompt pure
zstyle :prompt:pure:git:stash show yes

# zoxide
eval "$(zoxide init zsh)"

# navi
eval "$(navi widget zsh)"

# thefuck
eval $(thefuck --alias)

# fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# emacs-eat
[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
  source "$EAT_SHELL_INTEGRATION_DIR/zsh"

# emacs-vterm
if [[ "$INSIDE_EMACS" = 'vterm' ]] \
    && [[ -n ${EMACS_VTERM_PATH} ]] \
    && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh ]]; then
  source ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh
fi
