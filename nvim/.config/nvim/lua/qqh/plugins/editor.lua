return {
	{ -- auto pairs :)
		"windwp/nvim-autopairs",
		config = function()
			require("nvim-autopairs").setup({})
		end,
	},
	{ -- movements
		"smoka7/hop.nvim",
		version = "*",
		opts = {
			keys = "arstgmneioqwfpbjluyxcdvzkh",
		},
	},
	{ -- supercharged C-a and C-x
		"monaqa/dial.nvim",
		config = function()
			local augend = require("dial.augend")
			require("dial.config").augends:register_group({
				-- default augends used when no group name is specified
				default = {
					augend.integer.alias.decimal, -- nonnegative decimal number (0, 1, 2, 3, ...)
					augend.integer.alias.hex, -- nonnegative hex number  (0x01, 0x1a1f, etc.)
					augend.constant.alias.bool, -- boolean value (true <-> false)
					augend.date.alias["%Y/%m/%d"], -- date (2022/02/19, etc.)
					-- Make python True / False also cycle
					augend.constant.new({
						elements = { "True", "False" },
						word = true, -- if false, "sand" is incremented into "sor", "doctor" into "doctand", etc.
						cyclic = true, -- "or" is incremented into "and".
					}),
				},
			})
		end,
	},
	-- keymap
	"folke/which-key.nvim",
	{ -- scope buffers to a tab
		"tiagovla/scope.nvim",
		config = function()
			require("scope").setup({})
		end,
	},
	{ -- Highlight, edit, and navigate code
		"nvim-treesitter/nvim-treesitter",
		dependencies = {
			-- nushell parser
			{ "nushell/tree-sitter-nu" },
		},
		build = ":TSUpdate",
		opts = {
			textobjects = {
				select = {
					enable = true,
					lookahead = true,
					keymaps = {
						["aF"] = "@function.outer",
						["iF"] = "@function.inner",
					},
				},
			},
			-- A list of parser names, or "all"
			ensure_installed = {
				"bash",
				"c",
				"markdown",
				"markdown_inline",
				"lua",
				"luadoc",
				"rust",
				"vim",
				"vimdoc",
				"query",
				"python",
				"json",
				"yaml",
				"toml",
			},
			auto_install = true,
			highlight = {
				-- `false` will disable the whole extension
				enable = true,
			},
		},
		config = function(_, opts)
			-- Prefer git instead of curl in order to improve connectivity in some environments
			require("nvim-treesitter.install").prefer_git = true
			---@diagnostic disable-next-line: missing-fields
			require("nvim-treesitter.configs").setup(opts)

			-- There are additional nvim-treesitter modules that you can use to interact
			-- with nvim-treesitter. You should go explore a few and see what interests you:
			--
			--    - Incremental selection: Included, see `:help nvim-treesitter-incremental-selection-mod`
			--    - Show your current context: https://github.com/nvim-treesitter/nvim-treesitter-context
			--    - Treesitter + textobjects: https://github.com/nvim-treesitter/nvim-treesitter-textobjects
		end,
	},
	{ -- LSP
		"neovim/nvim-lspconfig",
		dependencies = {
			{ "williamboman/mason.nvim", config = true },
			"williamboman/mason-lspconfig.nvim",
			"WhoIsSethDaniel/mason-tool-installer.nvim",
			{
				"j-hui/fidget.nvim",
				opts = {
					notification = {
						window = { winblend = 0 },
					},
				},
			},
		},
	},
	{ -- formatting
		"stevearc/conform.nvim",
		opts = {
			formatters_by_ft = {
				lua = { "stylua" },
				python = { "black" },
				rust = { "rustfmt" },
				c = { "clang_format" },
				cpp = { "clang_format" },
				nix = { "alejandra" },
			},
			format_after_save = {
				-- I recommend these options. See :help conform.format for details.
				async = true,
				lsp_fallback = true,
				timeout_ms = 200,
			},
		},
	},
	{ -- Generate docstrings
		"danymat/neogen",
		dependencies = "nvim-treesitter/nvim-treesitter",
		config = true,
		version = "*",
	},
	-- language specific plugins
	{
		"NoahTheDuke/vim-just",
		event = { "BufReadPre", "BufNewFile" },
		ft = { "\\cjustfile", "*.just", ".justfile" },
	},
	{
		"mrcjkb/rustaceanvim",
		version = "^3", -- Recommended
		ft = { "rust" },
	},
	{
		"preservim/vim-markdown",
		config = function()
			vim.g.vim_markdown_folding_disabled = 1
		end,
	},
}
