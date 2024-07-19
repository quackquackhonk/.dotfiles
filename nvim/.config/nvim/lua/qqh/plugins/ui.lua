return {
	"tpope/vim-fugitive",
	"stevearc/dressing.nvim",
	{
		"stevearc/oil.nvim",
		opts = {
			columns = {
				"icon",
				"permissions",
				"size",
				"mtime",
			},
			use_default_keymaps = true,
			keymaps = {
				["g?"] = "actions.show_help",
				["<CR>"] = "actions.select",
				["<C-CR>"] = "actions.select_vsplit",
				["<S-CR>"] = "actions.select_split",
				["<C-t>"] = "actions.select_tab",
				["<C-p>"] = "actions.preview",
				["q"] = "actions.close",
				["<C-l>"] = "actions.refresh",
				["<BS>"] = "actions.parent",
				["_"] = "actions.open_cwd",
				["`"] = "actions.cd",
				["~"] = "actions.tcd",
				["gs"] = "actions.change_sort",
				["gx"] = "actions.open_external",
				["g."] = "actions.toggle_hidden",
				["g\\"] = "actions.toggle_trash",
			},
		},
	},
	{
		"declancm/maximize.nvim",
		opts = {},
	},
	-- Theme / UI
	{
		"folke/todo-comments.nvim",
		dependencies = { "nvim-lua/plenary.nvim" },
	},
	{ "catppuccin/nvim", name = "catppuccin", priority = 1000 },
	{
		"folke/trouble.nvim",
		config = function()
			require("trouble").setup({})
		end,
	},
	"norcalli/nvim-colorizer.lua",
	"lukas-reineke/indent-blankline.nvim",
	"HiPhish/rainbow-delimiters.nvim",
}
