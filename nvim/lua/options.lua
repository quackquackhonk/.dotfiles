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
    smartindent = true,
    wrap = false
}

for k, v in pairs(options) do
    vim.opt[k] = v
end
