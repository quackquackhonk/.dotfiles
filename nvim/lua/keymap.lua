local function keymap(mode, lhs, rhs)
    local options = { noremap = true, silent = true }
    vim.keymap.set(mode, lhs, rhs, options)
end

local wk = require('which-key')
local hydra = require('hydra')
local splits = require('smart-splits')
local cmd = require('hydra.keymap-util').cmd
local pcmd = require('hydra.keymap-util').pcmd

-- set up leader key
keymap("", "<Space>", "<Nop>")
vim.g.mapleader = ' '
vim.g.localmapleader = ' '

-- Leader key mappings with Which-Key
local telescope = require 'telescope.builtin'
local tele_themes = require 'telescope.themes'
local dap = require 'dap'
wk.register({
    -- f = buFfers
    -- e = Editor
    -- l = Language (sep in lsp.lua)
    -- d = Debugging
    -- g = git
    -- k = pacKer
    -- t = Test
    f = {
        name = "buFfers",
        f = { telescope.find_files, "Find File" },
        r = { telescope.oldfiles, "Recent Files" },
        n = { cmd("enew"), "New File" },
        s = { cmd("w"), "Save Buffer" },
        q = { cmd("bd"), "Close Buffer" },
        Q = { cmd("bd!"), "Force Close Buffer" },
    },
    e = {
        name = "Editor",
        p = { '"+p', "System Clipboard Paste" },
        P = { '"+P', "System Clipboard Paste Above" },
        y = { '"+y', "System Clipboard Yank" },
        f = "format file",
        h = { cmd("noh"), "Hide Highlighting" },
        u = { cmd("!dos2unix %"), "Dos2Unix current file" }
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
    g = {
        name = "Git Projects",
        g = { cmd("LazyGit"), "Git Status" },
        r = { cmd("Telescope projects"), "Recent Projects" }
    },
    d = {
        name = "Debugging",
        b = { dap.toggle_breakpoint, "Toggle Breakpoint" },
        c = { dap.continue, "DAP Continue" },
        r = { dap.repl.open, "Open DAP REPL" },
        o = { dap.step_over, "Step Over" },
        i = { dap.step_into, "Step Into" },
    },
    k = {
        name = "Packer",
        s = { cmd("PackerSync"), "Packer Sync" },
    },
    t = {
        name = "Tests",
        r = { ":lua require(\"neotest\").run.run()<cr>", "Run all tests" },
        f = { ":lua require(\"neotest\").run.run(vim.fn.expand(\"%\"))<cr>", "Run tests for current file" },
    },
    [","] = { "<c-6>", "Open Previous Buffer" },
    ["<Leader>"] = { ":Telescope buffers<cr>", "Show Open Buffers" },
    ["?"] = { cmd("Cheatsheet"), "Open Cheatsheet" }
}, { prefix = "<Leader>" })

-- HYDRA keymappings
-- Window move/resize hydra
hydra({
    name = 'Windows',
    hint = false,
    config = {
        invoke_on_body = false,
        hint = false,
    },
    timeout = true,
    mode = 'n',
    body = '<C-w>',
    heads = {
        { '<Left>', '<C-w>h' },
        { '<Down>', '<C-w>j' },
        { '<Up>', pcmd('wincmd k', 'E11', 'close') },
        { '<Right>', '<C-w>l' },

        { '<S-Left>', cmd 'WinShift left' },
        { '<S-Down>', cmd 'WinShift down' },
        { '<S-Up>', cmd 'WinShift up' },
        { '<S-Right>', cmd 'WinShift right' },

        { '<C-Left>', function() splits.resize_left(2) end },
        { '<C-Down>', function() splits.resize_down(2) end },
        { '<C-Up>', function() splits.resize_up(2) end },
        { '<C-Right>', function() splits.resize_right(2) end },
        { '=', '<C-w>=', { desc = 'equalize' } },

        { 's', pcmd('split', 'E36') },
        { '<C-s>', pcmd('split', 'E36'), { desc = false } },
        { 'v', pcmd('vsplit', 'E36') },
        { '<C-v>', pcmd('vsplit', 'E36'), { desc = false } },

        { 'q', pcmd('close', 'E444'), { desc = 'close window' } },

        { '<Esc>', nil, { exit = true, desc = false } }
    }
})

-- HOP keybindings
vim.api.nvim_set_keymap('', 'f',
    "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.AFTER_CURSOR, current_line_only = true })<cr>"
    , {})
vim.api.nvim_set_keymap('', 'F',
    "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.BEFORE_CURSOR, current_line_only = true })<cr>"
    , {})
vim.api.nvim_set_keymap('', 't',
    "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.AFTER_CURSOR, current_line_only = true, hint_offset = -1 })<cr>"
    , {})
vim.api.nvim_set_keymap('', 'T',
    "<cmd>lua require'hop'.hint_char1({ direction = require'hop.hint'.HintDirection.BEFORE_CURSOR, current_line_only = true, hint_offset = 1 })<cr>"
    , {})

-- DIAL keybindings
vim.api.nvim_set_keymap("n", "<C-a>", require("dial.map").inc_normal(), { noremap = true })
vim.api.nvim_set_keymap("n", "<C-x>", require("dial.map").dec_normal(), { noremap = true })
vim.api.nvim_set_keymap("v", "<C-a>", require("dial.map").inc_visual(), { noremap = true })
vim.api.nvim_set_keymap("v", "<C-x>", require("dial.map").dec_visual(), { noremap = true })
vim.api.nvim_set_keymap("v", "g<C-a>", require("dial.map").inc_gvisual(), { noremap = true })
vim.api.nvim_set_keymap("v", "g<C-x>", require("dial.map").dec_gvisual(), { noremap = true })

-- Convenience Keybindings
-- arrows for laptop
keymap('n', '×', '<Left>')
keymap('n', '–', '<Down>')
keymap('n', '€', '<Up>')
keymap('n', '—', '<Right>')
keymap('v', '×', '<Left>')
keymap('v', '–', '<Down>')
keymap('v', '€', '<Up>')
keymap('v', '—', '<Right>')
keymap('i', '×', '<Left>')
keymap('i', '–', '<Down>')
keymap('i', '€', '<Up>')
keymap('i', '—', '<Right>')
-- Toggles
keymap('n', '<F4>', ':NvimTreeToggle<cr>')
keymap('n', '<F5>', cmd("MaximizerToggle!"))
keymap('n', '<F6>', ':lua require("neotest").summary.toggle()<CR>')
keymap('n', '<F7>', ':TroubleToggle<cr>')
keymap('n', '<F8>', cmd("SymbolsOutline"))

-- Visual Mode
-- Stay in visual mode when indenting
keymap('v', '<', '<gv')
keymap('v', '>', '>gv')
