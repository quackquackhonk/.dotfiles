local options = {
    guicursor = "",
    nu = true,
    termguicolors = true,
    timeoutlen = 500,
    -- editor
    mouse = "a",
    cursorline = true,
    signcolumn = "yes",
    number = true,
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
