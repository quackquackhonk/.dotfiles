local function keymap(mode, lhs, rhs)
    local options = { noremap = true, silent = true }
    vim.keymap.set(mode, lhs, rhs, options)
end

local wk = require('which-key')
local hydra = require('hydra')
local splits = require('smart-splits')
local cmd = require('hydra.keymap-util').cmd
local pcmd = require('hydra.keymap-util').pcmd
local persistence = require('persistence')

-- set up leader key
keymap("", "<Space>", "<Nop>")
vim.g.mapleader = ' '
vim.g.maplocalleader = ','

local telescope = require 'telescope.builtin'
local ivy_telescope = function(func)
    local opt = require('telescope.themes').get_ivy {}
    return function() func(opt) end
end

local close_buf = function()
    require('bufdelete').bufdelete(0, true)
end

local rename_tab = function(input_str)
    vim.cmd("TabRename " .. input_str)
end

-- Leader key mappings with Which-Key
local dap = require 'dap'
wk.register({
    q = { close_buf, "Close Buffer" },
    Q = { cmd("bd"), "Close Buffer AND Window" },
    f = {
        name = "+find",
        f = { ivy_telescope(telescope.find_files), "Find Files" },
        g = { ivy_telescope(telescope.git_files), "Git Files" },
        r = { cmd("Telescope frecency theme=ivy"), "Recent Files" },
        s = { ivy_telescope(telescope.live_grep), "Live Grep" },
        p = { cmd("tabnew | Telescope project theme=ivy"), "Open Project" }
    },
    e = {
        name = "+editor",
        f = { vim.lsp.buf.format, "format file" },
        g = { cmd("cd %:h | cd `git rev-parse --show-toplevel`"), "CD to current git repo" },
        h = { cmd("noh"), "Hide Highlighting" },
        t = {
            name = "toggle",
            r = { cmd("set rnu!"), "relative numbers" }
        },
        u = { cmd("!dos2unix %"), "Dos2Unix current file" },
        y = { '"+y', "Copy to system clipboard" },
    },
    c = {
        name = "+code",
        v = { require('swenv.api').pick_venv, "Pick Virtual Environment" },
        c = { cmd("Neogen"), "Generate Comment" },
        t = {
            name = "+testing",
            r = { require('neotest').run.run, "Run nearest test" },
            f = { function() require('neotest').run.run(vim.fn.expand("%")) end, "Run tests in file" },
            o = { require('neotest').output.open, "Open test output" },
            O = { require('neotest').output_panel.toggle, "Toggle output panel" }
        },
    },
    t = {
        name = "tab",
        n = { cmd("tabnew"), "New Tab" },
        r = { function() vim.ui.input({ prompt = "Rename tab to..." }, rename_tab) end, "Rename current tab" },
    },
    l = {
        name = "+LSP",
        l = "Show Documentation",
        d = "Go To Definition",
        D = "Go To Declaration",
        t = "Go To Type Definition",
        i = "Go To Implementation",
        r = "Show References",
        c = "Change Symbol",
    },
    g = {
        name = "+git",
        g = { cmd("Neogit"), "Git Status" },
        b = { ivy_telescope(telescope.git_branches), "Show Branches" },
    },
    d = {
        name = "+debugging",
        b = { dap.toggle_breakpoint, "Toggle Breakpoint" },
        c = { dap.continue, "DAP Continue" },
        r = { dap.repl.open, "Open DAP REPL" },
        o = { dap.step_over, "Step Over" },
        i = { dap.step_into, "Step Into" },
        ["<Leader>"] = { vim.diagnostic.open_float, "Show diagnostic" },
        n = { vim.diagnostic.goto_next, "Next Diagnostic" },
        N = { vim.diagnostic.goto_prev, "Previous Diagnostic" },
    },
    o = {
        name = "+open",
        p = { cmd("lua MiniFiles.open()"), "Open project directory" },
        f = { cmd("lua MiniFiles.open(vim.api.nvim_buf_get_name(0), false)"), "Open current file directory" },
        d = { cmd("TroubleToggle"), "Diagnostics" },
        s = { cmd("SymbolsOutline"), "Symbol Outline" },
        t = { cmd("TodoTelescope theme=ivy"), "Show project TODOs" },
        o = { cmd("tabnew"), "Open tab" }
    },
    s = {
        name = "+settings",
        r = { cmd("set rnu!"), "Relative Numbers" },
    },
    [";"] = {
        name = "+neovim",
        p = { cmd("Lazy"), "Packages" },
        l = {
            name = "Load",
            l = { cmd("so %"), "Source current file" },
            t = { cmd("so ~/.dotfiles/nvim/lua/theme.lua"), "Source theme file" },
        },
        c = { cmd("tabnew | e ~/.config/nvim/init.lua | cd ~/.dotfiles/"), "Open Config" },
        s = {
            name = "+sessions",
            l = { persistence.load, "Load session for current dir" },
            s = { function() persistence.load({ last = true }) end, "Load previous session" },
            x = { persistence.stop, "Stop persistence" },
        },
        n = {
            name = "notifications",
            d = { cmd("NoiceDismiss"), "Dismiss notifications" },
            ["<Leader>"] = { cmd("Noice"), "Show message history" },
            e = { cmd("NoiceErrors"), "Show errors" },
        }
    },
    y = { cmd("Telescope neoclip theme=ivy"), "Open Neoclip" },
    [","] = { "<c-6>", "Open Previous Buffer" },
    ["<Tab>"] = { "<C-w>w", "Goto Previous Split" },
    ["<Leader>"] = { ivy_telescope(telescope.buffers), "Show Open Buffers" },
}, { prefix = "<Leader>" })

-- keymap('n', "<F6>", require("maximize").toggle)
-- keymap('i', "<F6>", require("maximize").toggle)
keymap('i', "<C-s>", "<Esc>")
keymap('i', "[t", "<cmd>tabprev<CR>")
keymap('i', "]t", "<cmd>tabnext<CR>")
keymap('n', "[t", ":tabprev<CR>")
keymap('n', "]t", ":tabnext<CR>")
keymap('i', "<C-q>", ":tabclose<CR>")
keymap('n', "<C-q>", ":tabclose<CR>")

-- C-arrows to move between windows
keymap('n', "<C-Left>", '<C-w>h')
keymap('n', "<C-Down>", '<C-w>j')
keymap('n', "<C-Up>", '<C-w>k')
keymap('n', "<C-Right>", '<C-w>l')
keymap('i', "<C-Left>", '<C-w>h')
keymap('i', "<C-Down>", '<C-w>j')
keymap('i', "<C-Up>", '<C-w>k')
keymap('i', "<C-Right>", '<C-w>l')

-- visual mode leader key bindings
wk.register({
    y = { '"+y', "Copy to system clipboard" },
    p = { '"+p', "Copy to system clipboard" },
}, { prefix = "<Leader>", mode = "v" })

-- HYDRA keymappings
-- Window move/resize hydra
-- TODO: Revamp how window management works

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
        { '<S-Left>',  cmd 'WinShift left' },
        { '<S-Down>',  cmd 'WinShift down' },
        { '<S-Up>',    cmd 'WinShift up' },
        { '<S-Right>', cmd 'WinShift right' },

        { 'm',         cmd('MaximizerToggle') },

        { '<C-Left>',  function() splits.resize_left(2) end },
        { '<C-Down>',  function() splits.resize_down(2) end },
        { '<C-Up>',    function() splits.resize_up(2) end },
        { '<C-Right>', function() splits.resize_right(2) end },
        { '=',         '<C-w>=',                             { desc = 'equalize' } },
        { 's',         pcmd('split', 'E36') },
        { '<C-s>',     pcmd('split', 'E36'),                 { desc = false } },
        { 'v',         pcmd('vsplit', 'E36') },
        { '<C-v>',     pcmd('vsplit', 'E36'),                { desc = false } },
        { 'q',         pcmd('close', 'E444'),                { desc = 'close window' } },
        { '<Esc>',     nil,                                  { exit = true, desc = false } }
    }
})

-- HOP keybinding-- place this in one of your configuration file(s)
local hop = require('hop')
local directions = require('hop.hint').HintDirection
vim.keymap.set('', 'f', function()
    hop.hint_char1({ direction = directions.AFTER_CURSOR, current_line_only = true })
end, { remap = true })
vim.keymap.set('', 'F', function()
    hop.hint_char1({ direction = directions.BEFORE_CURSOR, current_line_only = true })
end, { remap = true })
vim.keymap.set('', 't', function()
    hop.hint_char1({ direction = directions.AFTER_CURSOR, current_line_only = true, hint_offset = -1 })
end, { remap = true })
vim.keymap.set('', 'T', function()
    hop.hint_char1({ direction = directions.BEFORE_CURSOR, current_line_only = true, hint_offset = 1 })
end, { remap = true })
vim.keymap.set('', '<Return>', cmd('HopAnywhere'), { remap = true })

-- DIAL keybindings
vim.api.nvim_set_keymap("n", "<C-a>", require("dial.map").inc_normal(), { noremap = true })
vim.api.nvim_set_keymap("n", "<C-x>", require("dial.map").dec_normal(), { noremap = true })
vim.api.nvim_set_keymap("v", "<C-a>", require("dial.map").inc_visual(), { noremap = true })
vim.api.nvim_set_keymap("v", "<C-x>", require("dial.map").dec_visual(), { noremap = true })
vim.api.nvim_set_keymap("v", "g<C-a>", require("dial.map").inc_gvisual(), { noremap = true })
vim.api.nvim_set_keymap("v", "g<C-x>", require("dial.map").dec_gvisual(), { noremap = true })

-- Visual Mode
-- Stay in visual mode when indenting
keymap('v', '<', '<gv')
keymap('v', '>', '>gv')
