function keymap(mode, lhs, rhs)
    local options = { noremap = true, silent = true }
    vim.keymap.set(mode, lhs, rhs, options)
end

local wk = require('which-key')

-- set up leader key
keymap("", "<Space>", "<Nop>")
vim.g.mapleader = ' '
vim.g.localmapleader = ' '

-- Leader key mappings with Which-Key
local telescope = require'telescope.builtin'
wk.register({
    -- f = File
    -- e = Editor
    -- l = Language (sep in lsp.lua)
    -- g = git
    -- k = pacKer
    -- b = Buffer
    f = {
        name = "File",
        f = { telescope.find_files, "Find File" },
        r = { telescope.oldfiles, "Recent Files" },
        n = "New File"
    },
    e = {
        name = "Editor",
        p = {'"+p', "System Clipboard Paste" },
        P = {'"+P', "System Clipboard Paste Above" },
        y = {'"+y', "System Clipboard Yank" },
    },
    l = {
        name = "Language",
        l = "Show Documentation",
        d = "Go To Definition",
        D = "Go To Declaration",
        t = "Go To Type Definition",
        i = "Go To Implementation",
        r = "Show References",
        c = "Change Symbol"
    },
    d = {
        name = "Diagnostics",
        d = {"<cmd>Telescope diagnostics<cr>", "List Diagnostics"},
        n = {vim.diagnostic.goto_next, "Next Diagnostic"},
        N = {vim.diagnostic.goto_prev, "Previous Diagnostic"},
    },
    b = {
        name = "Buffer",
        s = {"<cmd>w<cr>", "Save"},
    },
    k = {
        name = "Packer",
        s = {":PackerSync<cr>", "Packer Sync"},
    },
    g = {
        name = "Git",
        g = {":LazyGit<cr>", "Git Status"},
    },
}, { prefix = "<Leader>" })

-- This needed to be in it's own section i think
wk.register({
    ["<Leader><Leader>"] = { telescope.buffers, "Show Open Buffers" }
})

-- Visual Mode
-- Stay in visual mode when indenting
keymap('v', '<', '<gv')
keymap('v', '>', '>gv')
