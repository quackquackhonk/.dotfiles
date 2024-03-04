vim.o.termguicolors = true
vim.g.mapleader = " "

-- install lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable", -- latest stable release
        lazypath,
    })
end
vim.opt.rtp:prepend(lazypath)

-- TODO: Define color palette in a single place


require("lazy").setup("plugins")
require("options")
require("lsp")
require("dap-conf")
require("keymap")
require("theme")
