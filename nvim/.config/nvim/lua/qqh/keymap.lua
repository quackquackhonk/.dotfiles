local wk = require("which-key")
local harpoon_telescope = require("qqh.harpoon")
local harpoon = require("harpoon")

wk.setup({
	preset = "helix",
	win = { border = "double" },
})

local cmd = function(str)
	return "<cmd>" .. str .. "<CR>"
end

local close_buf = function()
	require("mini.bufremove").delete()
end

local rename_tab = function(input_str)
	vim.cmd("TabRename " .. input_str)
end

local grep_open_files = function()
	builtin.live_grep({ grep_open_files = true })
end

wk.add({
	{ "<leader>f", group = "find" },
	{ "<leader>ff", cmd("Telescope find_files"), desc = "Find Files" },
	{ "<leader>fg", cmd("Telescope git_files"), desc = "Find Git Files" },
	{ "<leader>fa", cmd("AerialNavToggle"), desc = "Find Symbol in File" },
	{ "<leader>fs", cmd("Telescope live_grep"), desc = "Live Grep" },
	{ "<leader>fp", cmd("tabnew | Telescope project display_type=full"), desc = "Find project" },

	{ "<leader>c", group = "code" },
	{ "<leader>cm", cmd("Neogen"), desc = "Generate comment" },

	{ "<leader>o", group = "open" },
	{ "<leader>oo", cmd("OverseerToggle"), desc = "Open Overseer Window" },
	{ "<leader>of", cmd("Oil"), desc = "Open CWD in Oil" },
	{ "<leader>od", cmd("Trouble diagnostics toggle focus=true"), desc = "Open diagnostics window" },
	{ "<leader>ot", cmd("TodoTelescope keywords=TODO,Fix,FIXME"), desc = "Show project TODOs" },

	{ "<leader>e", group = "harpoon" },
	{
		"<leader>ea",
		function()
			harpoon:list():add()
		end,
		desc = "Add to Harpoon list",
	},
	{
		"<leader>en",
		function()
			harpoon:list():select(1)
		end,
		desc = "Goto Mark 1",
	},
	{
		"<leader>ee",
		function()
			harpoon:list():select(2)
		end,
		desc = "Goto Mark 2",
	},
	{
		"<leader>ei",
		function()
			harpoon:list():select(3)
		end,
		desc = "Goto Mark 3",
	},
	{
		"<leader>eo",
		function()
			harpoon:list():select(4)
		end,
		desc = "Goto Mark 4",
	},

	{ "<leader>t", group = "toggle" },
	{ "<leader>tr", cmd("set rnu!"), desc = "Toggle relative numbers" },
	{ "<leader>ts", cmd("set spell!"), desc = "Toggle Spellcheck" },
	{
		"<leader>tr",
		function()
			vim.ui.input({ prompt = "Rename tab to..." }, rename_tab)
		end,
		desc = "Rename current tab",
	},

	{ "<leader>;", group = "neovim" },
	{ "<leader>;n", cmd("tabnew"), desc = "New Tab" },
	{ "<leader>;c", cmd("tabnew | e ~/.config/nvim/init.lua | tcd ~/.dotfiles/"), desc = "Open neovim config" },
	{ "<leader>;s", cmd("so %"), desc = "Source current file" },
	{ "<leader>;n", cmd("tabnew | e ~/notes/index.norg | tcd ~/notes/"), desc = "Open Notes" },
	{ "<leader>;h", cmd("Telescope highlights"), desc = "Show Highlight Groups" },
	{ "<leader>;g", cmd("tcd %:h | tcd `git rev-parse --show-toplevel`"), desc = "CD to closest git repo root" },

	{ "<leader>w", group = "window" },
	{ "<leader>wv", cmd("vsplit"), desc = "Open vertical split" },
	{ "<leader>ws", cmd("split"), desc = "Open vertical split" },

	-- non-nested leader key
	{ "<leader>/", grep_open_files, desc = "Live grep in open files" },
	{ "<leader>?", cmd("Telescope help_tags"), desc = "Show help tags" },
	{ "<leader>q", close_buf, desc = "Close Buffer" },
	{ "<leader>Q", cmd("bd"), desc = "Close Buffer AND Window" },
	{ "<leader>k", vim.diagnostic.open_float, desc = "Show diagnostic" },
	{ "<leader>,", "<C-6>", desc = "Previous buffer" },
	{ "<leader>y", '"+y', desc = "Copy to system clipboard" },
	{ "<leader><Tab>", "<C-w>W", desc = "Goto last split" },
	{ "<leader><leader>", cmd("Telescope buffers"), desc = "Show open buffers" },
	{ "<leader>g", cmd("Git"), desc = "Open git status" },

	-- Normal mode mappings
	{ "<Esc>", cmd("nohlsearch"), desc = "Hide Highlighting" },
	{ "[t", cmd("tabprev"), desc = "Previous tab" },
	{ "]t", cmd("tabnext"), desc = "Next tab" },
	{ "<F8>", require("maximize").toggle, desc = "Maximize split" },
	{
		"<C-e>",
		function()
			harpoon_telescope({})
		end,
		desc = "Toggle harpoon window",
	},

	{
		mode = { "n", "v" },
		{ "<Return>", cmd("HopChar2"), desc = "Hop to character pair" },
		{ "<C-a>", require("dial.map").inc_normal(), desc = "Dial increment" },
		{ "<C-x>", require("dial.map").dec_normal(), desc = "Dial increment" },
	},

	{
		mode = { "n", "i" },
		{ "<C-Left>", "<C-w>h", desc = "Move focus left" },
		{ "<C-Right>", "<C-w>l", desc = "Move focus right" },
		{ "<C-Up>", "<C-w>k", desc = "Move focus up" },
		{ "<C-Down>", "<C-w>j", desc = "Move focus down" },
		{ "<C-q>", cmd("close"), desc = "Close window" },
	},
	{
		mode = { "n", "i", "v" },
		{ "<M-m>", "<Left>", desc = "Move left" },
		{ "<M-i>", "<Right>", desc = "Move right" },
		{ "<M-e>", "<Up>", desc = "Move up" },
		{ "<M-n>", "<Down>", desc = "Move down" },
	},

	{
		mode = { "v" },
		{ "<leader>y", '"+y', desc = "Copy to system clipboard" },
		{ "<leader>p", '"+p', desc = "Pase to system clipboard" },
		{ ">", ">gv" },
		{ "<", "<gv" },
	},
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

vim.api.nvim_set_keymap("v", "g<C-a>", require("dial.map").inc_gvisual(), { noremap = true })
vim.api.nvim_set_keymap("v", "g<C-x>", require("dial.map").dec_gvisual(), { noremap = true })
