return {
	{
		"smoka7/hop.nvim",
		version = "*",
		opts = {
			keys = "arstgmneioqwfpbjluyxcdvzkh",
		},
	},
	{
		"mizlan/iswap.nvim",
		event = "VeryLazy",
		opts = {
			keys = "arstgmneioqwfpbjluyxcdvzkh",
		},
	},
	{
		"mizlan/iswap.nvim",
		event = "VeryLazy",
		opts = {
			keys = "arstgmneioqwfpbjluyxcdvzkh",
		},
	},
	{
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
				},
			})
		end,
	},
	"tpope/vim-sensible",
	{
		"numToStr/Comment.nvim",
		config = function()
			require("Comment").setup()
		end,
	},
	{
		"windwp/nvim-autopairs",
		config = function()
			require("nvim-autopairs").setup({})
		end,
	},
	{
		"preservim/vim-markdown",
		config = function()
			vim.g.vim_markdown_folding_disabled = 1
		end,
	},
	-- keymap
	{
		"folke/which-key.nvim",
		config = function()
			require("which-key").setup({})
		end,
	},
	{
		"sudormrfbin/cheatsheet.nvim",
		config = function()
			require("cheatsheet").setup({})
		end,
	},
	"anuvyklack/hydra.nvim",
	"kevinhwang91/nvim-bqf",

	-- Buffer/Window Management
	{
		"tiagovla/scope.nvim",
		config = function()
			require("scope").setup({})
		end,
	},
	"famiu/bufdelete.nvim",
	"mrjones2014/smart-splits.nvim",
	"sindrets/winshift.nvim",
	{
		"s1n7ax/nvim-window-picker",
		name = "window-picker",
		event = "VeryLazy",
		version = "2.*",
		config = function()
			require("window-picker").setup()
		end,
	},
	{
		"numToStr/Navigator.nvim",
		config = function()
			require("Navigator").setup()
		end,
	},
	-- sessions
	{
		"folke/persistence.nvim",
		event = "BufReadPre",
		opts = {
			options = { "buffers", "curdir", "tabpages", "winsize", "globals" },
		},
	},
}
