return {
	-- Git Integration
	{
		"NeogitOrg/neogit",
		tag = "v0.0.1",
		dependencies = {
			"nvim-lua/plenary.nvim", -- required
			"sindrets/diffview.nvim", -- optional - Diff integration
			"nvim-telescope/telescope.nvim", -- optional
		},
		opts = {
			kind = "tab",
		},
	},
	-- copilot
	{
		"zbirenbaum/copilot.lua",
		config = function()
			require("copilot").setup({
				panel = { enabled = false },
				suggestion = { enabled = false },
			})
		end,
	},
	{
		"zbirenbaum/copilot-cmp",
		config = function()
			require("copilot_cmp").setup()
		end,
	},
	{
		"mfussenegger/nvim-dap",
		dependencies = {
			"nvim-telescope/telescope-dap.nvim",
			"rcarriga/nvim-dap-ui",
			"theHamsta/nvim-dap-virtual-text",
			"jbyuki/one-small-step-for-vimkind",
		},
	},
	{
		"stevearc/overseer.nvim",
		opts = {
			task_list = {
				default_detail = 1,
				direction = "right",
			},
			templates = { "builtin" },
		},
	},
	{
		"stevearc/aerial.nvim",
		dependencies = {
			"nvim-treesitter/nvim-treesitter",
			"nvim-tree/nvim-web-devicons",
		},
		opts = {
			layout = {
				width = 0.2,
			},
			nav = {
				keymaps = {
					["<Left>"] = "actions.left",
					["<Right>"] = "actions.right",
				},
			},
			-- optionally use on_attach to set keymaps when aerial has attached to a buffer
			on_attach = function(bufnr)
				-- Jump forwards/backwards with '{' and '}'
				vim.keymap.set("n", "[[", "<cmd>AerialPrev<CR>", { buffer = bufnr })
				vim.keymap.set("n", "]]", "<cmd>AerialNext<CR>", { buffer = bufnr })
			end,
		},
	},
	{
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
	},
	"neovim/nvim-lspconfig",
	{
		"williamboman/mason.nvim",
		config = function()
			require("mason").setup()
		end,
	},
	{
		"williamboman/mason-lspconfig.nvim",
		config = function()
			require("mason-lspconfig").setup()
		end,
	},
	-- Completion
	{
		"danymat/neogen",
		dependencies = "nvim-treesitter/nvim-treesitter",
		config = true,
		-- Uncomment next line if you want to follow only stable versions
		-- version = "*"
	},
	"hrsh7th/nvim-cmp",
	"hrsh7th/cmp-nvim-lsp",
	"hrsh7th/cmp-buffer",
	"hrsh7th/cmp-path",
	{
		"L3MON4D3/LuaSnip",
		version = "v2.*",
		run = "make install_jsregexp",
	},
	"saadparwaiz1/cmp_luasnip",
	"rafamadriz/friendly-snippets",
	"onsails/lspkind.nvim",
	{
		"AckslD/swenv.nvim",
		dependencies = "stevearc/dressing.nvim",
		config = function()
			require("swenv").setup({
				post_set_venv = function()
					vim.cmd.LspRestart()
				end,
			})
		end,
	},
	{
		"simrat39/symbols-outline.nvim",
		config = function()
			require("symbols-outline").setup({
				position = "right",
				keymaps = {
					close = "q",
					fold = "n",
					fold_all = "N",
					unfold = "e",
					unfold_all = "E",
				},
			})
		end,
	},
	{
		"stevearc/conform.nvim",
		opts = {
			formatters_by_ft = {
				lua = { "stylua" },
				python = { "isort", "black" },
				rust = { "rustfmt" },
				c = { "clang_format" },
				cpp = { "clang_format" },
			},
			format_on_save = {
				-- I recommend these options. See :help conform.format for details.
				lsp_fallback = true,
				timeout_ms = 500,
			},
		},
	},
	-- language specific plugins
	{
		"NoahTheDuke/vim-just",
		event = { "BufReadPre", "BufNewFile" },
		ft = { "\\cjustfile", "*.just", ".justfile" },
	},
	"mfussenegger/nvim-jdtls",
	{
		"nvim-neotest/neotest",
		dependencies = {
			"nvim-neotest/nvim-nio",
			"nvim-lua/plenary.nvim",
			"nvim-treesitter/nvim-treesitter",
			"nvim-neotest/neotest-python",
			"antoinemadec/FixCursorHold.nvim",
		},
		config = function()
			require("neotest").setup({
				adapters = {
					require("neotest-python"),
				},
			})
		end,
	},
	"folke/neodev.nvim",
	{
		"mrcjkb/rustaceanvim",
		version = "^3", -- Recommended
		ft = { "rust" },
	},
}
