local function keymap(mode, lhs, rhs)
	local options = { noremap = true, silent = true }
	vim.keymap.set(mode, lhs, rhs, options)
end

local wk = require("which-key")
wk.setup({ preset = "helix" })

local hydra = require("hydra")
local splits = require("smart-splits")
local pcmd = require("hydra.keymap-util").pcmd
local cmd = function(str)
	return "<cmd>" .. str .. "<CR>"
end

local telescope = require("telescope.builtin")
local close_buf = function()
	require("bufdelete").bufdelete(0, true)
end

local rename_tab = function(input_str)
	vim.cmd("TabRename " .. input_str)
end

local overseer = require("overseer")
vim.api.nvim_create_user_command("OverseerRestartOrRun", function()
	local tasks = overseer.list_tasks({ recent_first = true })
	if vim.tbl_isempty(tasks) then
		overseer.run_template({}, function(task)
			if task then
				overseer.run_action(task, "open tab")
			end
		end)
	else
		overseer.run_action(tasks[1], "restart")
	end
end, {})

-- Leader key mappings with Which-Key
wk.add({
	{ "<leader>f", group = "find" },
	{ "<leader>ff", cmd("Telescope find_files"), desc = "Find Files" },
	{ "<leader>fg", cmd("Telescope git_files"), desc = "Find Git Files" },
	{ "<leader>fa", cmd("AerialNavToggle"), desc = "Find Symbol in File" },
	{ "<leader>fs", cmd("Telescope live_grep"), desc = "Live Grep" },
	{ "<leader>fp", cmd("tabnew | Telescope project display_type=full"), desc = "Find project" },
	{ "<leader>fu", cmd("Telescope undo"), desc = "Find undo" },

	{ "<leader>c", group = "code" },
	{ "<leader>cm", cmd("Neogen"), desc = "Generate comment" },
	{ "<leader>cc", cmd("OverseerRestartOrRun"), desc = "Run a command" },

	{ "<leader>o", group = "open" },
	{ "<leader>oo", cmd("OverseerToggle"), desc = "Open Overseer Window" },
	{ "<leader>of", cmd("Oil"), desc = "Open CWD in Oil" },
	{ "<leader>od", cmd("Trouble diagnostics toggle focus=true"), desc = "Open diagnostics window" },
	{ "<leader>ot", cmd("TodoTelescope keywords=TODO,Fix,FIXME"), desc = "Show project TODOs" },

	{ "<leader>e", group = "editor" },
	{ "<leader>eh", cmd("Telescope highlights"), desc = "Show highlight groups" },
	{ "<leader>eg", cmd("tcd %:h | tcd `git rev-parse --show-toplevel`"), desc = "CD to closest git repo root" },
	{ "<leader>etr", cmd("set rnu!"), desc = "Toggle relative numbers" },

	{ "<leader>t", group = "tab" },
	{ "<leader>tn", cmd("tabnew"), desc = "New Tab" },
	{
		"<leader>tr",
		function()
			vim.ui.input({ prompt = "Rename tab to..." }, rename_tab)
		end,
		desc = "Rename current tab",
	},

	{ "<leader>;", group = "neovim" },
	{ "<leader>;c", cmd("tabnew | e ~/.config/nvim/init.lua | tcd ~/.dotfiles/"), desc = "Open neovim config" },
	{ "<leader>;s", cmd("so %"), desc = "Source current file" },
	{ "<leader>;n", cmd("tabnew | e ~/notes/index.norg | tcd ~/notes/"), desc = "Open Notes" },

	{ "<leader>w", group = "window" },
	{ "<leader>wv", cmd("vsplit"), desc = "Open vertical split" },
	{ "<leader>ws", cmd("split"), desc = "Open vertical split" },

	-- non-nested leader key
	{ "<leader>q", close_buf, desc = "Close Buffer" },
	{ "<leader>Q", cmd("bd"), desc = "Close Buffer AND Window" },
	{ "<leader>,", "<C-6>", desc = "Previous buffer" },
	{ "<leader>y", '"+y', desc = "Copy to system clipboard" },
	{ "<leader><Tab>", "<C-w>W", desc = "Goto last split" },
	{ "<leader><leader>", cmd("Telescope buffers"), desc = "Show open buffers" },

    -- Normal mode mappings
	{ "<Esc>", cmd("noh"), desc = "Hide Highlighting" },
	{ "[t", cmd("tabprev"), desc = "Previous tab" },
	{ "]t",  cmd("tabnext"), desc = "Next tab" },
    { "<F8>", require("maximize").toggle, desc = "Maximize split"},

    {
        mode = { "n", "v"}
    },

    {
        mode = { "n", "i"},
        { "<C-Left>", "<C-w>h", desc = "Move focus left"},
        { "<C-Right>", "<C-w>l", desc = "Move focus right"},
        { "<C-Up>", "<C-w>k", desc = "Move focus up"},
        { "<C-Down>", "<C-w>j", desc = "Move focus down"},
        { "<C-q>", cmd("close"), desc = "Close window" },
    },
    {
        mode = { "n", "i", "v"},
        { "<M-m>", "<Left>", desc = "Move left" },
        {"<M-i>", "<Right>", desc = "Move right" },
        {"<M-e>", "<Up>", desc = "Move up" },
        {"<M-n>", "<Down>", desc = "Move down" },
    },

    {
        mode = {"v"},
        { "<leader>y", '"+y', desc = "Copy to system clipboard" },
        { "<leader>p", '"+p', desc = "Pase to system clipboard" },
    }
})

-- Keybindings for HOP
-- place this in one of your configuration file(s)
local hop = require("hop")
local directions = require("hop.hint").HintDirection
vim.keymap.set("", "f", function()
	hop.hint_char1({ direction = directions.AFTER_CURSOR, current_line_only = true })
end, { remap = true })
vim.keymap.set("", "F", function()
	hop.hint_char1({ direction = directions.BEFORE_CURSOR, current_line_only = true })
end, { remap = true })
vim.keymap.set("", "t", function()
	hop.hint_char1({ direction = directions.AFTER_CURSOR, current_line_only = true, hint_offset = -1 })
end, { remap = true })
vim.keymap.set("", "T", function()
	hop.hint_char1({ direction = directions.BEFORE_CURSOR, current_line_only = true, hint_offset = 1 })
end, { remap = true })

-- DIAL keybindings
vim.api.nvim_set_keymap("n", "<C-a>", require("dial.map").inc_normal(), { noremap = true })
vim.api.nvim_set_keymap("v", "<C-a>", require("dial.map").inc_normal(), { noremap = true })
vim.api.nvim_set_keymap("n", "<C-x>", require("dial.map").dec_normal(), { noremap = true })
vim.api.nvim_set_keymap("v", "<C-x>", require("dial.map").dec_normal(), { noremap = true })
vim.api.nvim_set_keymap("v", "g<C-a>", require("dial.map").inc_gvisual(), { noremap = true })
vim.api.nvim_set_keymap("v", "g<C-x>", require("dial.map").dec_gvisual(), { noremap = true })
keymap({ "n", "v" }, "<Return>", cmd("HopChar2"))

-- Fold keymaps
vim.keymap.set("n", "<S-Tab>", require("fold-cycle").open, { silent = true, desc = "Fold-cycle: open folds" })
vim.keymap.set("n", "<Tab>", "za", { silent = true, desc = "Fold-cycle: open folds" })

-- Visual Mode
-- Stay in visual mode when indenting
keymap("v", "<", "<gv")
keymap("v", ">", ">gv")


-- HYDRA keymappings

-- Window move/resize hydra
hydra({
	name = "Windows",
	hint = false,
	config = {
		invoke_on_body = false,
		hint = false,
	},
	timeout = true,
	mode = "n",
	body = "<C-w>",
	heads = {
		{ "<S-Left>", cmd("WinShift left") },
		{ "<S-Down>", cmd("WinShift down") },
		{ "<S-Up>", cmd("WinShift up") },
		{ "<S-Right>", cmd("WinShift right") },

		{ "m", cmd("MaximizerToggle") },

		{
			"<Left>",
			function()
				splits.resize_left(2)
			end,
		},
		{
			"<Down>",
			function()
				splits.resize_down(2)
			end,
		},
		{
			"<Up>",
			function()
				splits.resize_up(2)
			end,
		},
		{
			"<Right>",
			function()
				splits.resize_right(2)
			end,
		},
		{ "=", "<C-w>=", { desc = "equalize" } },
		{ "<Esc>", nil, { exit = true, desc = false } },
	},
})
