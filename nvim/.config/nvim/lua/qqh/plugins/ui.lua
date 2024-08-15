return {
	{ "catppuccin/nvim", name = "catppuccin", priority = 1000 },
	"nvim-lualine/lualine.nvim",
	{ -- Command line
		"folke/noice.nvim",
		event = "VeryLazy",
		opts = {
			lsp = {
				-- override markdown rendering so that **cmp** and other plugins use **Treesitter**
				override = {
					["vim.lsp.util.convert_input_to_markdown_lines"] = true,
					["vim.lsp.util.stylize_markdown"] = true,
					["cmp.entry.get_documentation"] = true, -- requires hrsh7th/nvim-cmp
				},
			},
			messages = {
				enabled = true,
				view = "mini", -- default view for messages
				view_error = "mini", -- view for errors
				view_warn = "mini", -- view for warnings
				view_history = "messages", -- view for :messages
				view_search = false,
			},
			notify = {
				enabled = false,
				view = "mini",
			},
			presets = {
				bottom_search = true,
				long_message_to_split = true,
				lsp_doc_border = true,
			},
		},
		dependencies = {
			"MunifTanjim/nui.nvim",
		},
	},

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
	{ -- Maximize windows
		"declancm/maximize.nvim",
		opts = {},
	},
	{ -- Use telescope to search TODOs in the project
		"folke/todo-comments.nvim",
		dependencies = { "nvim-lua/plenary.nvim" },
	},
	{ -- breadcrumbs
		"utilyre/barbecue.nvim",
		name = "barbecue",
		version = "*",
		dependencies = {
			"SmiteshP/nvim-navic",
			"nvim-tree/nvim-web-devicons", -- optional dependency
		},
		opts = {
			theme = "catppuccin",
		},
	},
	{ -- Nice diagnostic view
		"folke/trouble.nvim",
		config = function()
			require("trouble").setup({})
		end,
	},
	{
		"nanozuki/tabby.nvim",
		event = "VimEnter", -- lazy load this shit
		dependencies = "nvim-tree/nvim-web-devicons",
	},
	{
		"lewis6991/gitsigns.nvim",
		opts = {},
	},
	{ -- database connections
		"kristijanhusak/vim-dadbod-ui",
		dependencies = {
			{ "tpope/vim-dadbod", lazy = true },
			{ "kristijanhusak/vim-dadbod-completion", ft = { "sql", "mysql", "plsql" }, lazy = true }, -- Optional
		},
		cmd = {
			"DBUI",
			"DBUIToggle",
			"DBUIAddConnection",
			"DBUIFindBuffer",
		},
		init = function()
			-- Your DBUI configuration
			vim.g.db_ui_execute_on_save = 0
			vim.g.db_ui_use_nvim_notify = 1

			vim.g.db_ui_win_position = "right"
			vim.g.db_ui_use_nerd_fonts = 1
		end,
	},
	"stevearc/dressing.nvim", -- better vim.ui.select
	"NvChad/nvim-colorizer.lua", -- highlight hex colors
	{
		"lukas-reineke/indent-blankline.nvim",
		main = "ibl",
		opts = {
			scope = {
				enabled = true,
				show_start = true,
				show_end = false,
			},
		},
	},
}
