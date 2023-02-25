vim.cmd 'source ~/.vimrc'

-- options {{{
vim.o.laststatus=2
vim.o.showmode=false
-- }}}

-- plug-declares {{{
require("paq"){
  'savq/paq-nvim';
  'nvim-lua/plenary.nvim';
  'nvim-tree/nvim-web-devicons';
  'feline-nvim/feline.nvim';
}
-- }}}

-- plug-configs {{{
require('feline').setup()
-- }}}
