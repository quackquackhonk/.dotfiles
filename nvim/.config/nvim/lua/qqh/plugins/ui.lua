return {
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
		-- Optional dependencies
		dependencies = { "nvim-tree/nvim-web-devicons" },
	},
	{
		"utilyre/barbecue.nvim",
		name = "barbecue",
		version = "*",
		dependencies = {
			"SmiteshP/nvim-navic",
			"nvim-tree/nvim-web-devicons", -- optional dependency
		},
		opts = {
			-- configurations go here
			theme = "catppuccin-mocha",
		},
	},
	{
		"declancm/maximize.nvim",
		opts = {},
	},
	-- Theme / UI
	{
		"jghauser/fold-cycle.nvim",
		opts = {},
	},
	{
		"folke/todo-comments.nvim",
		dependencies = { "nvim-lua/plenary.nvim" },
	},
	"nvim-lua/popup.nvim",
	"nvim-lua/plenary.nvim",
	"stevearc/dressing.nvim",
	"ellisonleao/gruvbox.nvim",
	{ "catppuccin/nvim", name = "catppuccin", priority = 1000 },
	"folke/lsp-colors.nvim",
	"nvim-lualine/lualine.nvim",
	{
		"folke/trouble.nvim",
		config = function()
			require("trouble").setup({})
		end,
	},
	"norcalli/nvim-colorizer.lua",
	{
		"m4xshen/smartcolumn.nvim",
		opts = {
			colorcolumn = "120",
			disabled_filetypes = {
				"help",
				"text",
				"markdown",
				"org",
				"lazy",
				"mason",
				"dashboard",
			},
			custom_colorcolumn = {
				python = "120",
			},
		},
	},
	"lukas-reineke/indent-blankline.nvim",
	"HiPhish/rainbow-delimiters.nvim",
	{

		"nanozuki/tabby.nvim",
		event = "VimEnter",
		dependencies = "nvim-tree/nvim-web-devicons",
	},
}
