local function keymap(mode, lhs, rhs)
	local options = { noremap = true, silent = true }
	vim.keymap.set(mode, lhs, rhs, options)
end

local wk = require("which-key")
local hydra = require("hydra")
local splits = require("smart-splits")
local cmd = require("hydra.keymap-util").cmd
local pcmd = require("hydra.keymap-util").pcmd
local persistence = require("persistence")

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
local dap = require("dap")
wk.register({
	q = { close_buf, "Close Buffer" },
	Q = { cmd("bd"), "Close Buffer AND Window" },
	f = {
		name = "+find",
		f = { cmd("Telescope find_files"), "Find Files" },
		g = { cmd("Telescope git_files"), "Git Files" },
		a = { cmd("AerialNavToggle"), "Symbol in file" },
		r = { cmd("Telescope frecency"), "Recent Files" },
		s = { cmd("Telescope live_grep"), "Live Grep" },
		p = { cmd("tabnew | Telescope project display_type=full"), "Find Project" },
		u = { cmd("Telescope undo"), "Find undo" },
	},
	e = {
		name = "+editor",
		f = { require("conform").format, "format file" },
		h = { cmd("Telescope highlights"), "Show highlight groups" },
		g = { cmd("tcd %:h | tcd `git rev-parse --show-toplevel`"), "CD to current git repo" },
		t = {
			name = "toggle",
			r = { cmd("set rnu!"), "relative numbers" },
		},
		s = { cmd("ISwap"), "Swap arguments" },
		u = { cmd("!dos2unix %"), "Dos2Unix current file" },
		y = { '"+y', "Copy to system clipboard" },
	},
	c = {
		name = "+code",
		v = { require("swenv.api").pick_venv, "Pick Virtual Environment" },
		m = { cmd("Neogen"), "Generate Comment" },
		c = { cmd("OverseerRestartOrRun"), "Run a command" },
		t = {
			name = "+testing",
			r = { require("neotest").run.run, "Run nearest test" },
			f = {
				function()
					require("neotest").run.run(vim.fn.expand("%"))
				end,
				"Run tests in file",
			},
			o = { require("neotest").output.open, "Open test output" },
			O = { require("neotest").output_panel.toggle, "Toggle output panel" },
		},
	},
	t = {
		name = "tab",
		n = { cmd("tabnew"), "New Tab" },
		r = {
			function()
				vim.ui.input({ prompt = "Rename tab to..." }, rename_tab)
			end,
			"Rename current tab",
		},
	},
	g = { cmd("Neogit"), "Git Status" },
	d = {
		name = "+debugging",
		b = { dap.toggle_breakpoint, "Toggle Breakpoint" },
		c = { dap.continue, "DAP Continue" },
		r = { dap.repl.open, "Open DAP REPL" },
		o = { dap.step_over, "Step Over" },
		i = { dap.step_into, "Step Into" },
	},
	o = {
		name = "+open",
		f = { cmd("Oil"), "Filebrowser" },
		d = { cmd("TroubleToggle"), "Diagnostics" },
		t = { cmd("TodoTelescope keywords=TODO,FIX,FIXME"), "Show project TODOs" },
		o = { cmd("OverseerToggle"), "Open Overseer window" },
	},
	w = {
		name = "window",
		v = { cmd("vsplit"), "Create vertical split" },
		s = { cmd("split"), "Create vertical split" },
		q = { cmd("close"), "Create vertical split" },
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
			t = { cmd("so ~/.dotfiles/nvim/lua/qqh/theme.lua"), "Source theme file" },
		},
		c = { cmd("tabnew | e ~/.config/nvim/init.lua | tcd ~/.dotfiles/"), "Open Config" },
		o = { cmd("tabnew | e ~/notes/index.norg | tcd ~/notes/"), "Open Notes" },
		n = {
			name = "notifications",
			d = { cmd("NoiceDismiss"), "Dismiss notifications" },
			["<Leader>"] = { cmd("Noice"), "Show message history" },
			e = { cmd("NoiceErrors"), "Show errors" },
		},
	},
	y = { cmd("Telescope neoclip"), "Open Neoclip" },
	[","] = { "<c-6>", "Open Previous Buffer" },
	["<Tab>"] = { "<C-w>W", "Focus previous split" },
	["<Leader>"] = { cmd("Telescope buffers"), "Show Open Buffers" },
}, { prefix = "<Leader>" })

-- Normal mode mappings
wk.register({
	["<Esc>"] = { cmd("noh"), "Hide Highlighting" },
	["[t"] = { cmd("tabprev"), "Previous tab" },
	["]t"] = { cmd("tabnext"), "Next tab" },
})

-- normal and insert mode mappings
wk.register({
	["<F8>"] = { require("maximize").toggle, "Maximize window" },
	["<C-q>"] = { cmd("close"), "Close window" },
	["<A-Left>"] = { cmd("NavigatorLeft"), "Move focus left" },
	["<A-Right>"] = { cmd("NavigatorRight"), "Move focus right" },
	["<A-Up>"] = { cmd("NavigatorUp"), "Move focus up" },
	["<A-Down>"] = { cmd("NavigatorDown"), "Move focus down" },
}, { mode = { "n", "i" } })

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

-- visual mode leader key bindings
wk.register({
	y = { '"+y', "Copy to system clipboard" },
	p = { '"+p', "Copy to system clipboard" },
}, { prefix = "<Leader>", mode = "v" })

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
