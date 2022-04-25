-- NOTE
-- :PackerClean 清除不用的插件
-- :PackerInstall 清除, 然后安装
-- :PackerSync 每次修改完该文件运行
-- :PackerLoad 立刻加载 opt 插件
local packer = require("packer")
packer.startup({
  function(use)
    -- Packer 可以管理自己本身
    use 'wbthomason/packer.nvim'
    -- theme tokyonight
    use 'folke/tokyonight.nvim'
    -- theme github-nvim
    use "projekt0n/github-nvim-theme"
    -- theme dracula
    use 'Mofiqul/dracula.nvim'
    -- indent-blankline
    use("lukas-reineke/indent-blankline.nvim")
    -- nvim-tree
    use({ "kyazdani42/nvim-tree.lua", requires = "kyazdani42/nvim-web-devicons" })
    -- bufferline
    use({ "akinsho/bufferline.nvim", requires = { "kyazdani42/nvim-web-devicons", "moll/vim-bbye" }})
    -- lualine 底部状态栏
    use({ "nvim-lualine/lualine.nvim", requires = { "kyazdani42/nvim-web-devicons" } })
    use("arkav/lualine-lsp-progress")
    -- telescope 浮窗搜索文件
    use { 'nvim-telescope/telescope.nvim', requires = { "nvim-lua/plenary.nvim" } }
    -- dashboard-nvim
    use("glepnir/dashboard-nvim")
    -- recent project
    use("ahmedkhalf/project.nvim")
    -- treesitter 代码高亮
    use({ "nvim-treesitter/nvim-treesitter", run = ":TSUpdate" })
    -- lspconfig
    use({ "neovim/nvim-lspconfig", "williamboman/nvim-lsp-installer" })
        -- lsp ui
    use("onsails/lspkind-nvim")
    use("tami5/lspsaga.nvim" )
    -- 补全引擎
    use("hrsh7th/nvim-cmp")
        -- snippet 引擎
    use("hrsh7th/vim-vsnip")
        -- 补全源
    use("hrsh7th/cmp-vsnip")
    use("hrsh7th/cmp-nvim-lsp") -- { name = nvim_lsp }
    use("hrsh7th/cmp-buffer") -- { name = 'buffer' },
    use("hrsh7th/cmp-path") -- { name = 'path' }
    use("hrsh7th/cmp-cmdline") -- { name = 'cmdline' }
        -- 常见编程语言代码段
    use("rafamadriz/friendly-snippets")
    -- copilot
    use("github/copilot.vim")
    -- surround.vim
    use("tpope/vim-surround") 
    -- easymotion, lightspeed
    use('ggandor/lightspeed.nvim')
    -- multiple cursors
    use('mg979/vim-visual-multi')
    -- TODO: 代码格式化, r, 

  end,
  config = {
    -- 并发数限制
    max_jobs = 16,
    -- 自定义源
    git = {
      -- default_url_format = "https://hub.fastgit.xyz/%s",
      -- default_url_format = "https://mirror.ghproxy.com/https://github.com/%s",
      -- default_url_format = "https://gitcode.net/mirrors/%s",
      -- default_url_format = "https://gitclone.com/github.com/%s",
    },
    -- display = {
    --     open_fn = require("packer.util").float,
    -- },
  },
})

-- 每次保存 plugins.lua 自动安装插件
pcall(
  vim.cmd,
  [[
    augroup packer_user_config
    autocmd!
    autocmd BufWritePost */nvim/lua/plugins.lua source <afile> | PackerSync
    augroup end
  ]]
)