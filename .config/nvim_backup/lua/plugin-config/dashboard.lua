-- use this link to generate http://patorjk.com/software/taag/#p=display&f=ANSI%20Shadow&t=neovim
vim.g.dashboard_default_executive = "telescope"
vim.g.dashboard_custom_footer = { "Yuchen Li" }
vim.g.dashboard_custom_section = {
    a = { description = { "  Projects              " }, command = "Telescope projects" },
    b = { description = { "  Recently files        " }, command = "Telescope oldfiles" },
    c = { description = { "  Edit Projects         " }, command = "edit ~/.local/share/nvim/project_nvim/project_history", },
    d = { description = { "  Edit keybindings      " }, command = "edit ~/.config/nvim/lua/keybindings.lua" },
    e = { description = { "  Edit .zshrc          " }, command = "edit ~/.zshrc" },
    f = { description = { "  Edit init.lua         " }, command = "edit ~/.config/nvim/init.lua" },
    g = { description = {'  Find file          '}, command = 'Telescope find_files'},
    h = { description = {'  Find text          '}, command = 'Telescope live_grep'},
  }

vim.g.dashboard_custom_header = {
    [[          ▀████▀▄▄              ▄█ ]],
    [[            █▀    ▀▀▄▄▄▄▄    ▄▄▀▀█ ]],
    [[    ▄        █          ▀▀▀▀▄  ▄▀  ]],
    [[   ▄▀ ▀▄      ▀▄              ▀▄▀  ]],
    [[  ▄▀    █     █▀   ▄█▀▄      ▄█    ]],
    [[  ▀▄     ▀▄  █     ▀██▀     ██▄█   ]],
    [[   ▀▄    ▄▀ █   ▄██▄   ▄  ▄  ▀▀ █  ]],
    [[    █  ▄▀  █    ▀██▀    ▀▀ ▀▀  ▄▀  ]],
    [[   █   █  █      ▄▄           ▄▀   ]],
  }