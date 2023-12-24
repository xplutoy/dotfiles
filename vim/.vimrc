vim9script

language en_US.utf-8

syntax on
filetype plugin indent on

g:mapleader = "\<space>"

set nocp
set bg=dark
set ruler
set title
set showcmd
set showmode
set shortmess=mrxoOtTIF
set emoji
set number
set relativenumber
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
set path+=/usr/local/include
set clipboard+=unnamed
set clipboard+=unnamedplus
set guioptions+=a
set hidden
set noautochdir
set autoread
set autowrite
set nobackup
set nowritebackup
set noswapfile
set autoindent
set smartindent
set breakindent
set tabstop=2
set shiftwidth=2
set softtabstop=2
set smarttab
set expandtab
set textwidth=0
set wrap wrapmargin=0
set linebreak showbreak=↪
set iskeyword+=_,$,@,%,#,-
set list listchars=tab:▸\ ,extends:❯,precedes:❮,nbsp:␣
set synmaxcol=200
set nrformats+=alpha
set splitbelow splitright
set ttimeout ttimeoutlen=100
set wildignore+=*/.git/*,*/__pycache__/*,*.DS_Store
set wildignorecase
set updatetime=500
set pastetoggle=<F2>
if has('mouse')
  set mouse=nic
  set mousemodel=popup
endif
if has('termguicolors')
  set termguicolors
endif
if exists('&inccommand')
  set inccommand=nosplit
endif

# cursor shape
&t_SI = "\<esc>[5 q"
&t_SR = "\<esc>[5 q"
&t_EI = "\<esc>[2 q"

noremap 0  ^
noremap ^  0
nnoremap x  "_x
xnoremap p  "_dP
xnoremap <  <gv
xnoremap >  >gv
inoremap <c-u> <c-g>u<c-u>
inoremap <c-w> <c-g>u<c-w>

imap <C-e> <esc>$a
imap <C-a> <esc>0i
imap <C-f> <esc>lli
imap <C-b> <esc>i
imap <C-k> <esc>d$i

map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

nnoremap <leader><leader> :nohl<cr>

# `b` is for buffer
map <leader>bb :buffers<cr>
map <leader>bd :bdelete<cr>
map <leader>bn :bnext<cr>
map <leader>bp :bprevious<cr>
# `t` is for tab
map <leader>tt :tabnew<cr>
map <leader>tn :tabnext<cr>
map <leader>tp :tabprevious<cr>
map <leader>to :tabonly<cr>
map <leader>tc :tabclose<cr>
map <leader>tm :tabmove
# `c` is for quickfix
map <leader>co :copen<cr>
map <leader>cc :cclose<cr>
map <leader>cn :cnext<cr>
map <leader>cp :cprevious<cr>

augroup user_cmds
  au!
  au Filetype man\|help nnoremap <buffer> q <cmd>quit<cr>
  au BufLeave,FocusLost,InsertEnter,WinLeave   * if &nu | set nornu | endif
  au BufEnter,FocusGained,InsertLeave,WinEnter * if &nu && mode() != "i" | set rnu   | endif
augroup END

runtime ftplugin/man.vim

plug#begin()
Plug 'junegunn/vim-peekaboo'
plug#end()
