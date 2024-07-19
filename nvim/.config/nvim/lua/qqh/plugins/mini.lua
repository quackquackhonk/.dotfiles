return {
	{
		"echasnovski/mini.nvim",
		version = false,
		config = function()
			require("mini.statusline").setup()
			require("mini.ai").setup()
			require("mini.bracketed").setup()
			require("mini.operators").setup()
			require("mini.surround").setup()

			require("mini.bufremove").setup()

			require("mini.icons").setup()
			require("mini.notify").setup()
			vim.notify = require("mini.notify").make_notify({
				ERROR = { duration = 4000 },
				WARN = { duration = 3000 },
				INFO = { duration = 2000 },
			})
		end,
	},
}
