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
alias ls='ls --color=auto'
alias ll='ls -lahF'
alias la='ls -A'
alias grep='grep --color=auto'
# emacs alias
alias ec='TERM=xterm-256color emacsclient -s server -t "$@"'
alias ecx='emacsclient -s server -n -c "$@"'
# alias esx='emacs --init-directory=~/.spacemacs.d/emacs.d "$@"'
# alias for git
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

alias r='radian'

# plugins
source $HOME/.zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source $HOME/.zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
fpath=($HOME/.zsh/plugins/zsh-completions/src $fpath); compinit
fpath+=($HOME/.zsh/pure); prompt pure
zstyle :prompt:pure:git:stash show yes

# zoxide
eval "$(zoxide init zsh)"

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
