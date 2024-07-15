return {
	{
		"nvim-telescope/telescope.nvim",
		branch = "0.1.x",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"debugloop/telescope-undo.nvim",
			"nvim-telescope/telescope-project.nvim",
			{
				"nvim-telescope/telescope-fzf-native.nvim",
				build = "make",
			},
		},
		config = function()
			vim.g.theme_switcher_loaded = true
			require("telescope").setup({
				pickers = {
					find_files = {
						hidden = true,
					},
				},
				defaults = {
					vimgrep_arguments = {
						"rg",
						"--color=never",
						"--no-heading",
						"--with-filename",
						"--follow",
						"--line-number",
						"--column",
						"--smart-case",
					},
					prompt_prefix = " > ",
					selection_caret = "  ",
					entry_prefix = "  ",
					initial_mode = "insert",
					selection_strategy = "reset",
					sorting_strategy = "ascending",
					layout_strategy = "bottom_pane",
					layout_config = {
						height = 0.4,
					},
					border = true,
					borderchars = {
						"z",
						prompt = { "─", " ", " ", " ", "─", "─", " ", " " },
						results = { " " },
						-- results = { "a", "b", "c", "d", "e", "f", "g", "h" },
						preview = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
					},
					file_sorter = require("telescope.sorters").get_fuzzy_file,
					file_ignore_patterns = { ".git/", "node_modules" },
					generic_sorter = require("telescope.sorters").get_generic_fuzzy_sorter,
					path_display = { "truncate" },
					winblend = 0,
					color_devicons = true,
					set_env = { ["COLORTERM"] = "truecolor" }, -- default = nil,
					file_previewer = require("telescope.previewers").vim_buffer_cat.new,
					grep_previewer = require("telescope.previewers").vim_buffer_vimgrep.new,
					qflist_previewer = require("telescope.previewers").vim_buffer_qflist.new,
					-- Developer configurations: Not meant for general override
					buffer_previewer_maker = require("telescope.previewers").buffer_previewer_maker,
					mappings = {
						i = {
							["<esc>"] = require("telescope.actions").close,
							["<C-d>"] = require("telescope.actions").delete_buffer,
						},
					},
				},
				extensions = {
					project = {
						base_dirs = {
							"~/code",
						},
					},
					fzf = {
						fuzzy = true, -- false will only do exact matching
						override_generic_sorter = true, -- override the generic sorter
						override_file_sorter = true, -- override the file sorter
						case_mode = "smart_case", -- or "ignore_case" or "respect_case"
					},
				},
			})
			require("telescope").load_extension("project")
			require("telescope").load_extension("fzf")
			require("telescope").load_extension("undo")
		end,
	},
	{
		"nvim-telescope/telescope-frecency.nvim",
		dependencies = { "kkharji/sqlite.lua" },
	},
}
