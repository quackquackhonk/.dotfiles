local options = {
    guicursor = "",
    nu = true,
    termguicolors = true,
    timeoutlen = 500,
    updatetime = 500,
    -- editor
    syntax = "on",
    mouse = "a",
    cursorline = true,
    signcolumn = "yes",
    number = true,
    relativenumber = true,
    -- foldexpr = "nvim_treesitter#foldexpr()",
    -- foldmethod = "expr",
    sessionoptions = "blank,buffers,curdir,folds,help,tabpages,winsize,winpos,terminal",
    -- tabs
    tabstop = 4,
    softtabstop = 4,
    shiftwidth = 4,
    expandtab = true,
    incsearch = true,
}

for k, v in pairs(options) do
    vim.opt[k] = v
end

vim.cmd('filetype plugin indent on')
vim.cmd('set t_ZH=[3m')
vim.cmd('set t_ZH=[23m')
