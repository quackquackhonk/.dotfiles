return {
	{
		"echasnovski/mini.nvim",
		version = false,
		config = function()
			-- Editing changes
			require("mini.ai").setup()
			require("mini.bracketed").setup({
				treesitter = { suffix = "", options = {} },
				undo = { suffix = "", options = {} },
			})
			require("mini.operators").setup()
			require("mini.surround").setup()

			-- remove buffers
			require("mini.bufremove").setup()

			-- UI
			require("mini.notify").setup()
			vim.notify = require("mini.notify").make_notify({
				ERROR = { duration = 5000 },
				WARN = { duration = 4000 },
				INFO = { duration = 3000 },
			})
			require("mini.icons").setup()
		end,
	},
}
