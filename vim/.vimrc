" setup {{{
language en_US.utf-8
let g:mapleader = "\<space>"                                      " use <SPC> as leader key
filetype plugin indent on                                     " enable filetype dectection and ft specific plugin/indent
syntax enable                                                 " enable syntax hightlight and completion
set nocp
set bg=dark
set ruler
set title                                                     " show file in titlebar
set showcmd                                                   " show typed command in status bar
set showmode
set shortmess=mrxoOtTIF
set emoji
set number                                                    " show line numbers
set relativenumber
set showmatch matchpairs+=<:>
set nocursorline                                                " 高亮当前行
set display+=lastline
set laststatus=1                                              " 0: hide 2:always
set signcolumn=auto
set noeb vb t_vb=                                             " no beep and no flash
set ttyfast                                                   " Faster redrawing.
set lazyredraw                                                " Only redraw when necessary.
set encoding=utf-8
set fileencoding=utf-8
set ffs=unix,dos,mac
set complete-=i                                               " Disable completing keywords in included files (perfermace)
set completeopt=noinsert,menu,noselect
set wrapscan
set incsearch hlsearch
set ignorecase smartcase                                      "only ignores case if there are no capital letters in search
set wildmenu                                                  " Show list instead of just completing
set wildmode=list:longest,full                                " Command <Tab> completion, list matches, then longest common part, then all.
set history=200
set nofoldenable foldmethod=marker
set backspace=indent,eol,start                                " More powerful backspacing
set linespace=0                                               " no extra spaces between rows
set report=0                                                  " always report number of lines changed"
set confirm                                                   " prompt when existing from an unsaved file
set scrolloff=8                                               " 8 lines above/below cursor when scrolling
set sidescroll=8
set matchtime=1                                               " show matching bracket for 0.2 seconds
set t_ti= t_te=                                               " 退出vim后，内容显示在终端屏幕，方便复制
set path+=/usr/local/include                                  " for gf find
set clipboard+=unnamed
set clipboard+=unnamedplus
set guioptions+=a
set hidden
set noautochdir
set autoread
set autowrite                                                 " 切换buffer时自动write
set nobackup
set nowritebackup
set autoindent
set smartindent
set breakindent
set tabstop=2
set shiftwidth=2
set softtabstop=2
set smarttab
set expandtab
set nowrap
set linebreak showbreak=↪
set iskeyword+=_,$,@,%,#,-                                    " 带有如下符号的单词不要被换行切割
set list listchars=tab:▸\ ,extends:❯,precedes:❮,nbsp:␣
set textwidth=78
set synmaxcol=200
set nrformats+=alpha
set splitbelow splitright
set ttimeout ttimeoutlen=100
set wildignore+=*/.git/*,*/__pycache__/*,*.DS_Store
set wildignorecase                                            " ignore file and dir name cases in cmd-completion
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
" }}}

" mappings {{{
map Q gq
sunmap Q
noremap 0  ^
noremap ^  0
nnoremap x  "_x
xnoremap p  "_dP
xnoremap <  <gv
xnoremap >  >gv
inoremap <c-u> <c-g>u<c-u>
inoremap <c-w> <c-g>u<c-w>

map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

nnoremap <leader><leader> :nohl<cr>
nnoremap <leader>ev  :edit $MYVIMRC<cr>

" `b` is for buffer
map <leader>bb :buffers<cr>
map <leader>bn :bnext<cr>
map <leader>bp :bprevious<cr>
map <leader>bd :bdelete<cr>
" `t` is for tab
map <leader>tt :tabnew<cr>
map <leader>tn :tabnext<cr>
map <leader>tp :tabprevious<cr>
map <leader>to :tabonly<cr>
map <leader>tc :tabclose<cr>
map <leader>tm :tabmove
" `c` is for quickfix
map <leader>co :copen<cr>
map <leader>cc :cclose<cr>
map <leader>cn :cnext<cr>
map <leader>cp :cprevious<cr>
" }}}

" commands {{{
command! W execute 'w !sudo tee % > /dev/null' <bar> edit!

augroup common
  au!
  autocmd BufReadPost * if line("'\"") >= 1 && line("'\"") <= line("$") && &ft !~# 'commit' | exe "normal! g`\"" | endif
  autocmd BufEnter,FocusGained,InsertLeave,WinEnter * if &nu && mode() != "i" | set rnu   | endif
  autocmd BufLeave,FocusLost,InsertEnter,WinLeave   * if &nu                  | set nornu | endif
augroup END
" }}}

" plug-declares {{{
if has('nvim')
  finish
endif
call plug#begin()
" builtin enhencement
Plug 'junegunn/vim-peekaboo'
Plug 'simnalamburt/vim-mundo', {'on': 'MundoToggle'}
" programing tools
Plug 'tpope/vim-fugitive'
call plug#end()
" }}}
" plug-keymaps {{{
map <leader>Tu :MundoToggle<cr>
" }}}

" plug-keymaps {{{

" }}}

" plug-configs {{{

" }}}
