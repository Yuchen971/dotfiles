-- basic config
require('basic')
-- keybindings
require('keybindings')
-- plugins
require('plugins')
require("plugin-config.nvim-tree")
require("plugin-config.bufferline")
require("plugin-config.lualine")
require("plugin-config.telescope")
require("plugin-config.dashboard")
require("plugin-config.project")
require("plugin-config.nvim-treesitter")
require("plugin-config.indent-blankline")
require("plugin-config.lightspeed")
-- lsp
require("lsp.setup")
require("lsp.cmp")
require("lsp.ui")
-- colorscheme
require("colorscheme")