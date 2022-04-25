--[[
lvim is the global options object

Linters should be
filled in as strings with either
a global executable or a path to
an executable
]]
-- THESE ARE EXAMPLE CONFIGS FEEL FREE TO CHANGE TO WHATEVER YOU WANT
-- Additional Plugins
lvim.plugins = {
  -- theme tokyonight
  { "folke/tokyonight.nvim" },
  -- theme dracula
  { 'Mofiqul/dracula.nvim' },
  -- lightspeed
  { "ggandor/lightspeed.nvim", event = "BufRead" },
  --- trouble
  { "folke/trouble.nvim",
    cmd = "TroubleToggle",
    require 'lightspeed'.setup {

    }
  },
  -- ranbow
  { "p00f/nvim-ts-rainbow" },
  {
    "tpope/vim-surround",
    keys = { "c", "d", "y" }
  },
  -- copilot
  { "github/copilot.vim" },
  -- glow
  {
    "npxbr/glow.nvim",
    ft = { "markdown" }
    -- run = "yay -S glow"
  },
  -- indent-blankline
  {
    "lukas-reineke/indent-blankline.nvim",
    event = "BufRead",
    setup = function()
      vim.g.indentLine_enabled = 1
      -- vim.g.indent_blankline_char = "▏"
      show_current_context = true
      vim.g.indent_blankline_char = "¦"
      vim.g.space_char_blankline = " "
      show_current_context_start = true
      vim.g.indent_blankline_filetype_exclude = { "help", "terminal", "dashboard" }
      vim.g.indent_blankline_buftype_exclude = { "terminal" }
      vim.g.indent_blankline_show_trailing_blankline_indent = false
      vim.g.indent_blankline_show_first_indent_level = false
    end
  },

  -- nvim-r
  { "jamespeapen/Nvim-R",
    vim.cmd("let R_app = 'radian'"),
    vim.cmd("let R_cmd = 'R'"),
    vim.cmd("let R_hl_term = 0"),
    vim.cmd("let R_args = []"),
    vim.cmd("let R_bracketed_paste = 1"),
  },
  -- lsp enhance
  {
    "rmagatti/goto-preview",
    config = function()
      require('goto-preview').setup {
        width = 120; -- Width of the floating window
        height = 25; -- Height of the floating window
        default_mappings = false; -- Bind default mappings
        debug = false; -- Print debug information
        opacity = nil; -- 0-100 opacity level of the floating window where 100 is fully transparent.
        post_open_hook = nil -- A function taking two arguments, a buffer and a window to be ran as a hook.
        -- You can use "default_mappings = true" setup option
        -- Or explicitly set keybindings
        -- vim.cmd("nnoremap gpd <cmd>lua require('goto-preview').goto_preview_definition()<CR>")
        -- vim.cmd("nnoremap gpi <cmd>lua require('goto-preview').goto_preview_implementation()<CR>")
        -- vim.cmd("nnoremap gP <cmd>lua require('goto-preview').close_all_win()<CR>")
      }
    end
  },
  {
    "ray-x/lsp_signature.nvim",
    event = "BufRead",
    config = function()
      require "lsp_signature".on_attach()
    end
  },
  {
    "simrat39/symbols-outline.nvim",
    cmd = "SymbolsOutline",
    setup = function()
      vim.g.symbols_outline = {
        highlight_hovered_item = false,
      }
    end
  },
  -- jupyter notebook
  { 'untitled-ai/jupyter_ascending.vim', },
  -- multi cursor
  { 'mg979/vim-visual-multi' }
}



-- general
vim.o.list = true
vim.o.listchars = "space:·"
vim.o.wildmenu = true
lvim.log.level = "warn"
lvim.format_on_save = true
lvim.colorscheme = "tokyonight"
-- keymappings [view all the defaults by pressing <leader>Lk]
lvim.leader = "space"
-- add your own keymapping
lvim.keys.normal_mode = {
  --quit
  ["qq"] = ":q<cr>",
  -- ["q"] = ":q<cr>",
  ["Q"] = ":qa!<cr>",
  -- ["<C-s>"] = ":w<cr>",

  -- move
  ["<C-j>"] = "4j",
  ["<C-k>"] = "4k",
  ["<C-u>"] = "9k",
  ["<C-d>"] = "9j",

  -- bufferline
  ["<C-h>"] = ":BufferLineCyclePrev<CR>",
  ["<C-l>"] = ":BufferLineCycleNext<CR>",

  -- terminal
  --- send current line
  ["<C-s>l"] = ":ToggleTermSendCurrentLine<CR>",

  -- jupyter notebook
  ["<space><space>x"] = "<Plug>JupyterExecute"

}
lvim.keys.insert_mode = {
  -- insert 模式下，跳到行首行尾
  ["<C-h>"] = "<ESC>I",
  ["<C-l>"] = "<ESC>A",
}

lvim.keys.visual_mode = {
  -- terminal
  --- send visual selection
  ["<C-s>v"] = ":ToggleTermSendVisualSelection<CR>",

}

-- unmap a default keymapping
-- lvim.keys.normal_mode["t"] = false
-- edit a default keymapping
-- lvim.keys.normal_mode["<C-q>"] = ":q<cr>"

-- terminal
lvim.builtin.terminal.direction = "horizontal"
lvim.builtin.terminal.size = 10
lvim.builtin.terminal.execs[#lvim.builtin.terminal.execs + 1] = { "vertical_terminal", "<leader>tv", "vertical_terminal" }


-- telescope
lvim.builtin.telescope.defaults.layout_config.preview_cutoff = 75

-- Change Telescope navigation to use j and k for navigation and n and p for history in both input and normal mode.
-- we use protected-mode (pcall) just in case the plugin wasn't loaded yet.
local _, actions = pcall(require, "telescope.actions")
lvim.builtin.telescope.defaults.mappings = {
  -- for input mode
  i = {
    ["<C-j>"] = actions.move_selection_next,
    ["<C-k>"] = actions.move_selection_previous,
    ["<C-n>"] = actions.cycle_history_next,
    ["<C-p>"] = actions.cycle_history_prev,
  },
  -- for normal mode
  n = {
    ["<C-j>"] = actions.move_selection_next,
    ["<C-k>"] = actions.move_selection_previous,
  },
}

-- Use which-key to add extra bindings with the leader-key prefix
-- lvim.builtin.which_key.mappings["P"] = { "<cmd>Telescope projects<CR>", "Projects" }

-- trouble
lvim.builtin.which_key.mappings["T"] = {
  name = "+Trouble",
  r = { "<cmd>Trouble lsp_references<cr>", "References" },
  f = { "<cmd>Trouble lsp_definitions<cr>", "Definitions" },
  d = { "<cmd>Trouble document_diagnostics<cr>", "Diagnostics" },
  q = { "<cmd>Trouble quickfix<cr>", "QuickFix" },
  l = { "<cmd>Trouble loclist<cr>", "LocationList" },
  w = { "<cmd>Trouble workspace_diagnostics<cr>", "Wordspace Diagnostics" },
}

-- bufferline
lvim.builtin.which_key.mappings["bc"] = {
  "<cmd>BufferLinePickClose<CR>", "close bufferline"
}

-- TODO: User Config for predefined plugins
-- After changing plugin config exit and reopen LunarVim, Run :PackerInstall :PackerCompile
lvim.builtin.alpha.active = true
lvim.builtin.alpha.mode = "dashboard"
lvim.builtin.notify.active = true
lvim.builtin.terminal.active = true
lvim.builtin.nvimtree.setup.view.side = "left"
lvim.builtin.nvimtree.show_icons.git = 0
lvim.builtin.dap.active = true

-- if you don't want all the parsers change this to a table of the ones you want
lvim.builtin.treesitter.ensure_installed = {
  "bash",
  "c",
  "javascript",
  "json",
  "lua",
  "python",
  "typescript",
  "tsx",
  "css",
  "rust",
  "java",
  "yaml",
  "r",
}

lvim.builtin.treesitter.ignore_install = { "haskell" }
lvim.builtin.treesitter.highlight.enabled = true

-- generic LSP settings
lvim.lsp.diagnostics.virtual_text = false
lvim.format_on_save = true
lvim.lsp.automatic_servers_installation = true
-- ---@usage disable automatic installation of servers
-- lvim.lsp.automatic_servers_installation = false

-- ---configure a server manually. !!Requires `:LvimCacheReset` to take effect!!
-- ---see the full default list `:lua print(vim.inspect(lvim.lsp.automatic_configuration.skipped_servers))`
-- vim.list_extend(lvim.lsp.automatic_configuration.skipped_servers, { "pyright" })
-- local opts = {} -- check the lspconfig documentation for a list of all possible options
-- require("lvim.lsp.manager").setup("pyright", opts)

-- ---remove a server from the skipped list, e.g. eslint, or emmet_ls. !!Requires `:LvimCacheReset` to take effect!!
-- ---`:LvimInfo` lists which server(s) are skiipped for the current filetype
-- vim.tbl_map(function(server)
--   return server ~= "emmet_ls"
-- end, lvim.lsp.automatic_configuration.skipped_servers)

-- -- you can set a custom on_attach function that will be used for all the language servers
-- -- See <https://github.com/neovim/nvim-lspconfig#keybindings-and-completion>
-- lvim.lsp.on_attach_callback = function(client, bufnr)
--   local function buf_set_option(...)
--     vim.api.nvim_buf_set_option(bufnr, ...)
--   end
--   --Enable completion triggered by <c-x><c-o>
--   buf_set_option("omnifunc", "v:lua.vim.lsp.omnifunc")
-- end

-- set a formatter, this will override the language server formatting capabilities (if it exists)
local formatters = require "lvim.lsp.null-ls.formatters"
formatters.setup {
  { command = "black",
    filetypes = { "python" },
  },
  {
    command = "prettier",
    ---@usage specify which filetypes to enable. By default a providers will attach to all the filetypes it supports.
    -- filetypes = { "typescript", "typescriptreact" },
  }
}

-- Autocommands (https://neovim.io/doc/user/autocmd.html)
-- lvim.autocommands.custom_groups = {
-- { "BufWinEnter", "*.lua", "setlocal ts=8 sw=8" },
-- }
