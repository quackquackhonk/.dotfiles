local colors = require("catppuccin.palettes").get_palette("mocha")

colors.white = colors.text
colors.black = colors.base
colors.gray = colors.overlay1
colors.grey = colors.gray
colors.purple = colors.mauve
colors.aqua = colors.teal
colors.orange = colors.peach

require("catppuccin").setup({
	flavour = "mocha",
	background = {
		light = "latte",
		dark = "mocha",
	},
	show_end_of_buffer = true,
	dim_inactive = {
		enabled = true,
		shade = "dark",
		percentage = 0,
	},
	styles = {
		conditionals = {},
	},
	custom_highlights = function(colors)
		return {
			WinSeparator = { bg = colors.mantle, fg = colors.mantle },
			VertSplit = { bg = colors.mantle, fg = colors.mantle },
			MiniIndentscopeSymbol = { fg = colors.surface1 },
			-- tabline
			TabLine = { bg = colors.mantle },
			TabLineFill = { bg = colors.mantle },
			TabLineSel = { bg = colors.surface0, fg = colors.rosewater },

			-- Telescope
			TelescopeNormal = { fg = colors.blue, bg = colors.crust },
			TelescopeBorder = { fg = colors.blue, bg = colors.crust },
			TelescopePromptBorder = { fg = colors.blue, bg = colors.crust },
			TelescopeSelection = { bg = colors.base },
			TelescopeMatching = { fg = colors.pink },

			-- completion menu
			CmpItemKindSnippet = { fg = colors.base, bg = colors.mauve },
			CmpItemKindKeyword = { fg = colors.base, bg = colors.red },
			CmpItemKindText = { fg = colors.base, bg = colors.teal },
			CmpItemKindMethod = { fg = colors.base, bg = colors.blue },
			CmpItemKindConstructor = { fg = colors.base, bg = colors.blue },
			CmpItemKindFunction = { fg = colors.base, bg = colors.blue },
			CmpItemKindFolder = { fg = colors.base, bg = colors.blue },
			CmpItemKindModule = { fg = colors.base, bg = colors.blue },
			CmpItemKindConstant = { fg = colors.base, bg = colors.peach },
			CmpItemKindField = { fg = colors.base, bg = colors.green },
			CmpItemKindProperty = { fg = colors.base, bg = colors.green },
			CmpItemKindEnum = { fg = colors.base, bg = colors.green },
			CmpItemKindUnit = { fg = colors.base, bg = colors.green },
			CmpItemKindClass = { fg = colors.base, bg = colors.yellow },
			CmpItemKindVariable = { fg = colors.base, bg = colors.flamingo },
			CmpItemKindFile = { fg = colors.base, bg = colors.blue },
			CmpItemKindInterface = { fg = colors.base, bg = colors.yellow },
			CmpItemKindColor = { fg = colors.base, bg = colors.red },
			CmpItemKindReference = { fg = colors.base, bg = colors.red },
			CmpItemKindEnumMember = { fg = colors.base, bg = colors.red },
			CmpItemKindStruct = { fg = colors.base, bg = colors.blue },
			CmpItemKindValue = { fg = colors.base, bg = colors.peach },
			CmpItemKindEvent = { fg = colors.base, bg = colors.blue },
			CmpItemKindOperator = { fg = colors.base, bg = colors.blue },
			CmpItemKindTypeParameter = { fg = colors.base, bg = colors.blue },
			CmpItemKindCopilot = { fg = colors.base, bg = colors.teal },
		}
	end,
	integrations = {
		cmp = true,
		fidget = true,
		mason = true,
		mini = {
			enabled = true,
		},
		hop = true,
		native_lsp = {
			enabled = true,
			virtual_text = {
				errors = {},
				hints = {},
				warnings = {},
				information = {},
			},
			underlines = {
				errors = { "underline" },
				hints = { "underline" },
				warnings = { "underline" },
				information = { "underline" },
			},
			inlay_hints = {
				background = true,
			},
		},
		which_key = true,
	},
})
vim.opt.termguicolors = true
vim.cmd("colorscheme catppuccin-mocha")

-- Todo comments
require("todo-comments").setup({
	keywords = {
		-- FIXME: testing text
		FIX = {
			icon = " ", -- icon used for the sign, and in search results
			color = "error", -- can be a hex color, or a named color (see below)
			alt = { "FIXME", "BUG", "FIXIT", "ISSUE" }, -- a set of other keywords that all map to this FIX keywords
		},
		-- TODO: testing text
		TODO = { icon = " " },
		-- HACK: testing text
		HACK = { icon = " ", color = "error" },
		-- WARN: testing text
		WARN = { icon = " ", alt = { "WARNING", "XXX" } },
		-- PERF: testing text
		PERF = { icon = " ", alt = { "OPTIM", "PERFORMANCE", "OPTIMIZE" } },
		-- NOTE: testing text
		NOTE = { icon = " ", color = "hint", alt = { "INFO" } },
	},
})

require("tabby").setup({
	preset = "tab_only",
	option = {
		nerdfont = false,
		lualine_theme = nil,
		tabname = {
			name_fallback = function(tabid)
				return tabid
			end,
		},
	},
})

require("lualine").setup({
	theme = "catppuccin",
	options = {
		component_separators = "|",
		section_separators = { left = "", right = "" },
	},
})
