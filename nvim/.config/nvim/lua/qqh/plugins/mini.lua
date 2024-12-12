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
			require("mini.icons").setup()
		end,
	},
}
