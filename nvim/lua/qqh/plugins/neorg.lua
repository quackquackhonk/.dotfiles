return {
	{
		"vhyrro/luarocks.nvim",
		priority = 1000,
		config = true,
	},
	{
		"nvim-neorg/neorg",
		lazy = false,
		version = "*",
		dependencies = {
			-- "nvim-lua/plenary.nvim",
			"luarocks.nvim",
			"nvim-neorg/neorg-telescope",
		},
		config = function()
			require("neorg").setup({
				load = {
					["core.defaults"] = {}, -- Loads default behaviour
					["core.concealer"] = {}, -- Adds pretty icons to your documents
					["core.dirman"] = { -- Manages Neorg workspaces
						config = {
							workspaces = {
								notes = "~/Dropbox/",
							},
						},
					},
					["core.integrations.telescope"] = {},
					["core.keybinds"] = {
						config = {
							default_keybinds = false,
							hook = function(keybinds)
								local leader = keybinds.leader
								keybinds.map_event_to_mode("norg", {
									n = {
										-- Marks the task under the cursor as "undone"
										{
											leader .. "tu",
											"core.qol.todo_items.todo.task_undone",
											opts = { desc = "[neorg] Mark as Undone" },
										},

										-- Marks the task under the cursor as "pending"
										{
											leader .. "tp",
											"core.qol.todo_items.todo.task_pending",
											opts = { desc = "[neorg] Mark as Pending" },
										},

										-- Marks the task under the cursor as "done"
										{
											leader .. "td",
											"core.qol.todo_items.todo.task_done",
											opts = { desc = "[neorg] Mark as Done" },
										},

										-- Marks the task under the cursor as "cancelled"
										{
											leader .. "tc",
											"core.qol.todo_items.todo.task_cancelled",
											opts = { desc = "[neorg] Mark as Cancelled" },
										},

										-- Switches the task under the cursor between a select few states
										{
											leader .. "t<Space>",
											"core.qol.todo_items.todo.task_cycle",
											opts = { desc = "[neorg] Cycle Task" },
										},

										-- Creates a new .norg file to take notes in
										-- ^New Note
										{
											leader .. "nn",
											"core.dirman.new.note",
											opts = { desc = "[neorg] Create New Note" },
										},

										-- Hop to the destination of the link under the cursor
										{
											leader .. "<CR>",
											"core.esupports.hop.hop-link",
											opts = { desc = "[neorg] Jump to Link" },
										},

										-- Same as `<CR>`, except opens the destination in a vertical split
										{
											leader .. "<M-CR>",
											"core.esupports.hop.hop-link",
											"vsplit",
											opts = { desc = "[neorg] Jump to Link (Vertical Split)" },
										},

										{
											">.",
											"core.promo.promote",
											opts = { desc = "[neorg] Promote Object (Non-Recursively)" },
										},
										{
											"<,",
											"core.promo.demote",
											opts = { desc = "[neorg] Demote Object (Non-Recursively)" },
										},

										{
											">>",
											"core.promo.promote",
											"nested",
											opts = { desc = "[neorg] Promote Object (Recursively)" },
										},
										{
											"<<",
											"core.promo.demote",
											"nested",
											opts = { desc = "[neorg] Demote Object (Recursively)" },
										},

										{
											leader .. "lt",
											"core.pivot.toggle-list-type",
											opts = { desc = "[neorg] Toggle (Un)ordered List" },
										},
										{
											leader .. "li",
											"core.pivot.invert-list-type",
											opts = { desc = "[neorg] Invert (Un)ordered List" },
										},

										{
											leader .. "id",
											"core.tempus.insert-date",
											opts = { desc = "[neorg] Insert Date" },
										},
										{
											leader .. "li",
											"core.integrations.telescope.insert_link",
											opts = { desc = "[neorg] Insert link" },
										},
										{
											leader .. "lf",
											"core.integrations.telescope.insert_file_link",
											opts = { desc = "[neorg] Insert file link" },
										},
									},

									i = {
										{
											"<Tab>",
											"core.promo.promote",
											opts = { desc = "[neorg] Promote Object (Recursively)" },
										},
										{
											"<S-Tab>",
											"core.promo.demote",
											opts = { desc = "[neorg] Demote Object (Recursively)" },
										},
										{
											"<M-CR>",
											"core.itero.next-iteration",
											"<CR>",
											opts = { desc = "[neorg] Continue Object" },
										},
										{
											"<M-d>",
											"core.tempus.insert-date-insert-mode",
											opts = { desc = "[neorg] Insert Date" },
										},
									},
								}, {
									silent = true,
									noremap = true,
								})
							end,
						},
					},
					["core.completion"] = {
						config = {
							engine = "nvim-cmp",
						},
					},
					["core.integrations.nvim-cmp"] = {},
				},
			})
		end,
	},
}
