return {
	{
		"nvim-neorg/neorg",
		config = function()
			require("neorg").setup({
				load = {
					["core.defaults"] = {},
					["core.export"] = {},
					["core.concealer"] = {},
					["core.dirman"] = {
						config = {
							workspaces = {
								notes = "~/notes",
							},
							default_workspace = "notes",
						},
					},
					["core.keybinds"] = {
						config = {
							default_keybinds = false,
							hook = function(keybinds)
								-- neorg defaults to using the LocalLeader key
								local leader = keybinds.leader

								keybinds.map_event_to_mode("norg", {
									n = {
										-- Mark task as undone
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

										-- Marks the task under the cursor as "on_hold"
										{
											leader .. "th",
											"core.qol.todo_items.todo.task_on_hold",
											opts = { desc = "[neorg] Mark as On Hold" },
										},

										-- Marks the task under the cursor as "cancelled"
										{
											leader .. "tc",
											"core.qol.todo_items.todo.task_cancelled",
											opts = { desc = "[neorg] Mark as Cancelled" },
										},

										--
										-- LINK NAVIGATION
										--
										-- Hop to the destination of the link under the cursor
										{
											"<CR>",
											"core.esupports.hop.hop-link",
											opts = { desc = "[neorg] Jump to Link" },
										},
										{
											"<M-CR>",
											"core.esupports.hop.hop-link",
											"vsplit",
											opts = { desc = "[neorg] Jump to Link (Vertical Split)" },
										},

										--
										-- PROMOTION
										--
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
									},
								})
							end,
						},
					},
				},
			})
		end,
	},
}
