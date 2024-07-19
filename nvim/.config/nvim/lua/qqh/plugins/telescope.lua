return {
	{
		"nvim-telescope/telescope.nvim",
		branch = "0.1.x",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-telescope/telescope-project.nvim",
			{
				"nvim-telescope/telescope-fzf-native.nvim",
				build = "make",
				cond = function()
					return vim.fn.executable("make") == 1
				end,
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
					file_ignore_patterns = { ".git/", "node_modules" },
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
			pcall(require("telescope").load_extension("project"))
			pcall(require("telescope").load_extension("fzf"))
		end,
	},
}
