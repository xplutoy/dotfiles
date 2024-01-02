vim9script

set nocp
set bg=dark
syntax enable
filetype plugin indent on
language en_US.utf-8

set ruler
set title
set emoji
set magic
set hidden
set t_Co=256
set showcmd
set showmode
set mouse=nic
set mousemodel=popup
set number
set relativenumber
set shortmess=mrxoOtTIF
set showmatch matchpairs+=<:>
set nocursorline
set display+=lastline
set laststatus=2
set signcolumn=auto
set noeb vb t_vb=
set ttyfast
set lazyredraw
set encoding=utf-8
set fileencoding=utf-8
set ffs=unix,dos,mac
set complete-=i
set completeopt=noinsert,menu,noselect
set wrapscan
set incsearch hlsearch
set ignorecase smartcase
set wildmenu
set wildmode=list:longest,full
set history=200
set nofoldenable foldmethod=marker
set backspace=indent,eol,start
set linespace=0
set report=0
set confirm
set scrolloff=8
set sidescroll=8
set matchtime=1
set clipboard+=unnamed
set clipboard+=unnamedplus
set guioptions+=a
set noautochdir
set autoread
set autowrite
set nobackup
set nowritebackup
set noswapfile
set autoindent
set smartindent
set breakindent
set shiftwidth=2
set softtabstop=2
set smarttab
set expandtab
set wrap wrapmargin=0
set linebreak showbreak=↪
set iskeyword+=_,$,@,%,#,-
set synmaxcol=200
set nrformats+=alpha
set splitbelow splitright
set ttimeout ttimeoutlen=100
set updatetime=500
set wildignore+=*/.git/*
set wildignore+=*/__pycache__/*
set wildignorecase
set grepprg=rg\ --vimgrep\ --smart-case\ --follow
set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+

# cursor shape
&t_SI = "\<esc>[5 q"
&t_SR = "\<esc>[5 q"
&t_EI = "\<esc>[2 q"

g:mapleader = "\<space>"
g:maplocalleader = ","

noremap 0  ^
noremap ^  0
nnoremap x  "_x
xnoremap p  "_dP
xnoremap <  <gv
xnoremap >  >gv

nnoremap K :Man <cword><cr>

inoremap <C-e> <esc>$a
inoremap <C-a> <esc>0i
inoremap <C-f> <esc>lli
inoremap <C-b> <esc>i
inoremap <C-k> <esc>d$i

noremap <leader>c :copen<cr>
noremap <leader>t :tabnew<cr>
noremap <leader>b :buffers<cr>

nnoremap <silent> <C-l> :<C-u>noh<cr><C-l>

command W :execute ':silent w !sudo tee % > /dev/null' | :edit!

augroup user_cmds
  au!
  au InsertEnter * if &nu | set nornu | endif
  au InsertLeave * if &nu | set rnu   | endif
  au InsertLeave,WinEnter * set cursorline
  au InsertEnter,WinLeave * set nocursorline
  au Filetype man,help nnoremap <buffer> q <cmd>quit<cr>
augroup END

# packadd! editorconfig
runtime ftplugin/man.vim

plug#begin()
Plug 'junegunn/vim-peekaboo'
plug#end()
