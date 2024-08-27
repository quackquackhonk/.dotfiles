local wk = require("which-key")
local hop = require("hop")
local hop_directions = require("hop.hint").HintDirection

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

local rename_tab = function()
	vim.ui.input({ prompt = "Rename tab..." }, function(input)
		vim.cmd("TabRename " .. input)
	end)
end

local function open_gitui()
	local wd = vim.uv.cwd()
	if os.getenv("ZELLIJ") ~= nil then
		vim.system({
			"zellij",
			"run",
			"-f",
			"-c",
			"--cwd",
			wd,
			"--x",
			"5%",
			"--y",
			"5%",
			"--height",
			"90%",
			"--width",
			"90%",
			"--",
			"gitui",
		})
	else
		vim.cmd("Git")
	end
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
	{ "<leader>oo", cmd("Trouble symbols toggle focus=true win.position=bottom"), desc = "Open Symbol Outline" },
	{ "<leader>os", cmd("tabnew | DBUI"), desc = "Open Database UI" },
	{ "<leader>of", cmd("Oil"), desc = "Open CWD in Oil" },
	{ "<leader>od", cmd("Trouble diagnostics toggle focus=true"), desc = "Open diagnostics window" },
	{ "<leader>ot", cmd("TodoTelescope keywords=TODO,Fix,FIXME"), desc = "Show project TODOs" },

	{ "<leader>t", group = "toggle" },
	{ "<leader>tr", cmd("set rnu!"), desc = "Toggle relative numbers" },
	{ "<leader>ts", cmd("set spell!"), desc = "Toggle Spellcheck" },
	{ "<leader>tc", cmd("CoverageToggle"), desc = "Toggle Code Coverage" },

	{ "<leader>;", group = "neovim" },
	{ "<leader>;c", cmd("tabnew | e ~/.config/nvim/init.lua | tcd ~/.dotfiles/"), desc = "Open neovim config" },
	{ "<leader>;s", cmd("so %"), desc = "Source current file" },
	{ "<leader>;n", cmd("tabnew | e ~/notes/index.norg | tcd ~/notes/"), desc = "Open Notes" },
	{ "<leader>;h", cmd("Telescope highlights"), desc = "Show Highlight Groups" },
	{ "<leader>;g", cmd("tcd %:h | tcd `git rev-parse --show-toplevel`"), desc = "CD to closest git repo root" },
	{ "<leader>;t", group = "tabs" },
	{ "<leader>;tn", cmd("tabnew"), desc = "New Tab" },
	{ "<leader>;tr", rename_tab, desc = "New Tab" },

	{ "<leader>w", group = "window" },
	{ "<leader>wv", cmd("vsplit"), desc = "Open vertical split" },
	{ "<leader>ws", cmd("split"), desc = "Open vertical split" },

	-- non-nested leader key
	{ "<leader>?", cmd("Telescope help_tags"), desc = "Show help tags" },
	{ "<leader>q", close_buf, desc = "Close Buffer" },
	{ "<leader>Q", cmd("bd"), desc = "Close Buffer AND Window" },
	{ "<leader>k", vim.diagnostic.open_float, desc = "Show diagnostic" },
	{ "<leader>,", "<C-6>", desc = "Previous buffer" },
	{ "<leader>y", '"+y', desc = "Copy to system clipboard" },
	{ "<leader><Tab>", "<C-w>W", desc = "Goto last split" },
	{ "<leader><leader>", cmd("Telescope buffers"), desc = "Show open buffers" },
	{ "<leader>g", open_gitui, desc = "Open gitui" },

	-- Quick tabs
	{ "<leader>1", "1gt", desc = "Focus Tab 1" },
	{ "<leader>2", "2gt", desc = "Focus Tab 2" },
	{ "<leader>3", "3gt", desc = "Focus Tab 3" },
	{ "<leader>4", "4gt", desc = "Focus Tab 4" },
	{ "<leader>5", "5gt", desc = "Focus Tab 5" },
	{ "<leader>6", "6gt", desc = "Focus Tab 6" },
	{ "<leader>7", "7gt", desc = "Focus Tab 7" },
	{ "<leader>8", "8gt", desc = "Focus Tab 8" },
	{ "<leader>9", "9gt", desc = "Focus Tab 9" },

	-- Normal mode mappings
	{ "<Esc>", cmd("nohlsearch"), desc = "Hide Highlighting" },
	{ "<C-a>", require("dial.map").inc_normal(), desc = "Dial increment" },
	{ "<C-x>", require("dial.map").dec_normal(), desc = "Dial increment" },

	-- bracketed movement
	-- Tabs
	{ "]t", cmd("tabnext"), desc = "Next tab" },
	{ "[t", cmd("tabprev"), desc = "Previous tab" },
	{
		"]u",
		function()
			require("coverage").jump_next("uncovered")
		end,
		desc = "Next uncovered line",
	},
	-- Uncovered lines
	{
		"[u",
		function()
			require("coverage").jump_prev("uncovered")
		end,
		desc = "Previous uncovered line",
	},

	{ "<F8>", require("maximize").toggle, desc = "Maximize split" },

	{
		mode = { "n", "v" },
		-- HOP keybindings
		{
			"f",
			function()
				hop.hint_char1({ direction = hop_directions.AFTER_CURSOR, current_line_only = true })
			end,
		},
		{
			"t",
			function()
				hop.hint_char1({ direction = hop_directions.AFTER_CURSOR, current_line_only = true, hint_offset = -1 })
			end,
		},
		{
			"F",
			function()
				hop.hint_char1({ direction = hop_directions.BEFORE_CURSOR, current_line_only = true })
			end,
		},
		{
			"T",
			function()
				hop.hint_char1({ direction = hop_directions.BEFORE_CURSOR, current_line_only = true, hint_offset = -1 })
			end,
		},
		{ "<Return>", cmd("HopChar2"), desc = "Hop to character pair" },
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
		-- stay in visual mode when indenting
		{ ">", ">gv" },
		{ "<", "<gv" },
		-- visual dial
		{ "g<C-a>", require("dial.map").inc_gvisual() },
		{ "g<C-x>", require("dial.map").dec_gvisual() },
	},
})
