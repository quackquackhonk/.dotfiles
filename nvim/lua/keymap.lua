local function keymap(mode, lhs, rhs)
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
        name = "buFfers",
        f = { telescope.find_files, "Find File" },
        r = { telescope.oldfiles, "Recent Files" },
        n = { ":enew<cr>", "New File" },
        s = { ":w<cr>", "Save Buffer" },
        q = { ":bd<cr>", "Close Buffer" },
        Q = { ":bd!<cr>", "Force Close Buffer"},
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
        c = "Change Symbol",
    },
    d = {
        name = "Diagnostics",
        d = {"<cmd>Telescope diagnostics<cr>", "List Diagnostics"},
        n = {vim.diagnostic.goto_next, "Next Diagnostic"},
        N = {vim.diagnostic.goto_prev, "Previous Diagnostic"},
    },
    k = {
        name = "Packer",
        s = {":PackerSync<cr>", "Packer Sync"},
    },
    g = {
        name = "Git Projects",
        g = {":LazyGit<cr>", "Git Status"},
        r = {":Telescope projects<cr>", "Recent Projects"}
    },
    [","] = {"<c-6>", "Open Previous Buffer"},
    ["<Leader>"] = { telescope.buffers, "Show Open Buffers"},
}, { prefix = "<Leader>" })


-- Visual Mode
-- Stay in visual mode when indenting
keymap('v', '<', '<gv')
keymap('v', '>', '>gv')
