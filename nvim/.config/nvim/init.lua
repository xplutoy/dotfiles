-- [[ options ]]
vim.opt.wrap = true
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.breakindent = true
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.updatetime = 250
vim.opt.undofile = true
vim.opt.winblend = 5
vim.opt.clipboard = 'unnamedplus'
vim.opt.signcolumn = 'auto'
vim.opt.background = "dark"
vim.opt.termguicolors = true
vim.opt.completeopt = {'menu','menuone','noselect'}

vim.g.netrw_banner = 0
vim.g.netrw_winsize = 30

-- [[ keybingdings ]]
vim.g.mapleader = ' '
vim.g.maplocalleader = ','

local opts = {
  silent = true,
  noremap = true,
}

vim.keymap.set('x', '<', '<gv', opts)
vim.keymap.set('x', '>', '>gv', opts)

vim.keymap.set({'n', 'x'}, 'x',  '"_x', opts)
vim.keymap.set({'n', 'x'}, 'gy', '"+y', opts)
vim.keymap.set({'n', 'x'}, 'gp', '"+p', opts)

vim.keymap.set('n', '<C-l>', ':noh<cr><C-l>', opts)
vim.keymap.set({'n', 'x'}, '<leader>t', ':tabnew<cr>', opts)

-- [[ commands ]]
vim.api.nvim_create_user_command('ReloadConfig', 'source $MYVIMRC', {})
local group = vim.api.nvim_create_augroup('user_cmds', {clear = true})
vim.api.nvim_create_autocmd(
  'FileType',
  { group = group,
    pattern = {'help', 'man'},
    command = 'nnoremap <buffer> q <cmd>quit<cr>'
  })
vim.api.nvim_create_autocmd(
  'InsertEnter',
  { group = group,
    callback = function() if vim.opt.number:get() then vim.opt.relativenumber = false end end, }
)
vim.api.nvim_create_autocmd(
  'InsertLeave',
  { group = group,
    callback = function() if vim.opt.number:get() then vim.opt.relativenumber = true  end end, }
)

-- [[ plugins ]]
local function clone_paq()
  local path = vim.fn.stdpath("data") .. "/site/pack/paqs/start/paq-nvim"
  local is_installed = vim.fn.empty(vim.fn.glob(path)) == 0
  if not is_installed then
    vim.fn.system { "git", "clone", "--depth=1", "https://github.com/savq/paq-nvim.git", path }
    return true
  end
end

local function bootstrap_paq(packages)
  local first_install = clone_paq()
  vim.cmd.packadd("paq-nvim")
  local paq = require("paq")
  if first_install then
    vim.notify("Installing plugins... If prompted, hit Enter to continue.")
  end
  paq(packages)
  paq.install()
end

bootstrap_paq {
  'savq/paq-nvim',

  'folke/which-key.nvim',

  'nvim-treesitter/nvim-treesitter',
  'neovim/nvim-lspconfig',

  'honza/vim-snippets',
  'dcampos/nvim-snippy',
  'dcampos/cmp-snippy',

  'hrsh7th/cmp-path',
  'hrsh7th/cmp-buffer',
  'hrsh7th/cmp-nvim-lsp',
  'hrsh7th/nvim-cmp',

  'ibhagwan/fzf-lua',
}

-- theme
vim.o.background = "dark"
-- vim.cmd([[colorscheme gruvbox]])

-- which-key
require('which-key').setup{}

-- snippy
require('snippy').setup({
  mappings = {
    is = {
      ['<Tab>'] = 'expand_or_advance',
      ['<S-Tab>'] = 'previous',
    },
  },
})

-- treesitter
require('nvim-treesitter.configs').setup({
  auto_install = false,
  ensure_installed = {'c', 'python', 'julia', 'lua', 'vim', 'vimdoc'},
  indent = { enable = true },
  highlight = { enable = true, },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = '<c-space>',
      node_incremental = '<c-space>',
      scope_incremental = '<c-s>',
      node_decremental = '<M-space>',
    },
  },
})

vim.opt.foldenable = false
vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "nvim_treesitter#foldexpr()"

-- lsp
vim.api.nvim_create_autocmd('LspAttach', {
  group = group,
  callback = function(ev)
    -- manual completion <c-x><c-o>
    vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'
    local opts = { buffer = ev.buf }
    vim.keymap.set('n', '<F2>', vim.lsp.buf.rename, opts)
    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
    vim.keymap.set('n', 'K',  vim.lsp.buf.hover, opts)
    vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
    vim.keymap.set('n', 'go', vim.lsp.buf.type_definition, opts)
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
    vim.keymap.set('n', 'gs', vim.lsp.buf.signature_help, opts)
    vim.keymap.set('n', 'ga', vim.lsp.buf.code_action, opts)
    vim.keymap.set('x', 'ga', vim.lsp.buf.code_action, opts)
    vim.keymap.set('n', 'gl', vim.diagnostic.open_float, opts)
    vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
    vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
  end,
})

local lspconfig = require('lspconfig')
local lsp_capabilities = require('cmp_nvim_lsp').default_capabilities()
require'lspconfig'.lua_ls.setup{capabilities = lsp_capabilities}
require'lspconfig'.pylsp.setup{capabilities = lsp_capabilities}
require'lspconfig'.julials.setup{capabilities = lsp_capabilities}
require'lspconfig'.clangd.setup{capabilities = lsp_capabilities}

-- nvim-cmp
local cmp = require'cmp'
cmp.setup({
  sources = {
    {name = 'nvim_lsp'},
    {name = 'path'},
    {name = 'buffer'},
    {name = 'nvim_lsp'},
  },
  snippet = {
    expand = function(args)
      require('snippy').expand_snippet(args.body) -- For `snippy` users.
    end,
  },
  window = {
    completion = {
      border = "rounded",
      scrollbar = false,
      winhighlight = "Normal:Pmenu",
    },
  },
  mapping = cmp.mapping.preset.insert({
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-u>'] = cmp.mapping.scroll_docs(-4),
    ['<C-d>'] = cmp.mapping.scroll_docs(4),
    ['<C-e>'] = cmp.mapping.abort(),
    ['<C-p>'] = cmp.mapping.select_prev_item(),
    ['<C-n>'] = cmp.mapping.select_next_item(),
    ['<CR>']  = cmp.mapping.confirm({select = true}),
  })
})

-- fzf-lua

local fzf = require('fzf-lua') ; fzf.setup{}
vim.keymap.set('n', '<leader>b', fzf.buffers, {})
vim.keymap.set('n', '<leader>f', fzf.files, {})
vim.keymap.set('n', '<leader>r', fzf.oldfiles, {})
vim.keymap.set('n', '<leader>l', fzf.lines, {})
vim.keymap.set('n', '<leader>g', fzf.live_grep, {})
vim.keymap.set('n', '<leader>h', fzf.help_tags, {})
vim.keymap.set('n', '<leader>v', fzf.git_status, {})
vim.keymap.set('n', '<leader>a', fzf.lsp_code_actions, {})
vim.keymap.set('n', '<leader>R', fzf.lsp_references, {})
vim.keymap.set('n', '<leader>d', fzf.lsp_definitions, {})
vim.keymap.set('n', '<leader>s', fzf.lsp_document_symbols, {})
