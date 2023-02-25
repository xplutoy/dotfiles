export ZDOTDIR=$HOME                        # default:$HOME
export EDITOR='vim'
export VISUAL='vim'
export YX_DOTDIR=$HOME/dot/
export YX_ORGDIR=$HOME/personal/org/
export PASSWORD_STORE_DIR=$HOME/personal/password-store/
export GPG_TTY=$(tty)

export PATH=~/.local/bin:$PATH

fpath=($YX_DOTDIR/zsh.d $fpath)

# line edit
bindkey -e
export KEYTIMEOUT=1

# history settings
if [ -z $HISTFILE ]; then
    HISTFILE=$HOME/.zsh_history
fi
export HISTSIZE=10000                       # Maximum events for internal history
export SAVEHIST=10000                       # Maximum events in history file
setopt INC_APPEND_HISTORY                   # Write to the history file immediately,
setopt SHARE_HISTORY                        # Share history between all sessions.
setopt APPEND_HISTORY                       # append to history file
setopt HIST_SAVE_NO_DUPS                    # Do not write a duplicate event to the history file.

# dir stack
setopt AUTO_PUSHD                           # Push the current directory visited on the stack.
setopt PUSHD_IGNORE_DUPS                    # Do not store duplicates in the stack.
setopt PUSHD_SILENT                         # Do not print the directory stack after pushd or popd

# 别名
alias cp='cp -i'
alias mv='mv -i'
alias :q=exit
alias ..='cd ..'
alias ls='ls --color=auto'
alias ll='ls -la'
alias grep='grep --color=auto'
alias em='emacs -q -l ~/dot/emacs.d/init-mini.el -nw "$@"'
alias emx='emacs -q -l ~/dot/emacs.d/init-mini.el "$@"'
alias ec='TERM=xterm-256color emacsclient -s server -t "$@"'
alias ecx='emacsclient -s server -n -c "$@"'
alias esx='emacs --no-desktop --init-directory=~/.spacemacs.d/.emacs.d "$@"'
alias notmuch-delete='notmuch search --output=files tag:deleted | xargs rm'

# alias for git
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
alias gstl='git stash list'
alias gsta='git stash push'
alias gstaa='git stash apply'
alias gstd='git stash drop'
alias gstp='git stash pop'
alias gstc='git stash clear'

autoload -U colors && colors
autoload -U compinit; compinit

unsetopt MENU_COMPLETE
setopt AUTO_MENU
setopt ALWAYS_TO_END
setopt COMPLETE_IN_WORD
setopt NOBEEP
setopt CORRECT_ALL

zstyle ':completion:*' completer _extensions _complete _approximate
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "$ZDOTDIR/.zcompcache"
zstyle ':completion:*' menu select
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' file-sort change

# 插件管理器
[ -f $YX_DOTDIR/miniplug/miniplug.zsh ] && source $YX_DOTDIR/miniplug/miniplug.zsh
# plugin managed
miniplug plugin 'zsh-users/zsh-syntax-highlighting'
miniplug plugin 'zsh-users/zsh-autosuggestions'
miniplug plugin 'zsh-users/zsh-completions'
miniplug plugin 'agkozak/zsh-z'
miniplug plugin 'Tarrasch/zsh-bd'
miniplug plugin 'sindresorhus/pure'
miniplug load

# 插件配置
# pure theme
fpath+=($HOME/.local/share/miniplug/sindresorhus/pure)
autoload -U promptinit; promptinit
prompt pure
zstyle :prompt:pure:git:stash show yes

# emacs vterm shell side configuration
if [[ "$INSIDE_EMACS" = 'vterm' ]] \
    && [[ -n ${EMACS_VTERM_PATH} ]] \
    && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh ]]; then
	source ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh
fi

# zoxide
eval "$(zoxide init zsh)"

# fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

# sdcv
export STARDICT_DATA_DIR="$HOME/.local/share/stardict/dic/"

# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
