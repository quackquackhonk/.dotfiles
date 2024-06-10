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
			-- tabline
			TabLine = { bg = colors.mantle },
			TabLineFill = { bg = colors.mantle },

			-- Telescope
			TelescopeNormal = { fg = colors.blue, bg = colors.crust },
			TelescopeBorder = { fg = colors.blue, bg = colors.crust },
			TelescopePromptBorder = { fg = colors.blue, bg = colors.crust },
			TelescopeSelection = { bg = colors.base },
			TelescopeMatching = { fg = colors.pink },

			-- TSContext
			TreesitterContext = { bg = colors.base },

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

			-- Folding UI
			Folded = { bg = colors.mantle },
		}
	end,
	integrations = {
		cmp = true,
		barbecue = {
			alt_background = false,
		},
		mason = true,
		mini = {
			enabled = true,
		},
		hop = true,
		neogit = true,
		neotest = true,
		noice = true,
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
				background = false,
			},
		},
		which_key = true,
	},
})
vim.opt.termguicolors = true
vim.cmd("colorscheme catppuccin-mocha")

-- Rainbow Delimiters / indent blankline setup
local highlight = {
	"RainbowRed",
	"RainbowYellow",
	"RainbowBlue",
	"RainbowGreen",
	"RainbowViolet",
	"RainbowCyan",
}
local hooks = require("ibl.hooks")
-- create the highlight groups in the highlight setup hook, so they are reset
-- every time the colorscheme changes
hooks.register(hooks.type.HIGHLIGHT_SETUP, function()
	vim.api.nvim_set_hl(0, "RainbowRed", { fg = colors.red })
	vim.api.nvim_set_hl(0, "RainbowYellow", { fg = colors.yellow })
	vim.api.nvim_set_hl(0, "RainbowBlue", { fg = colors.blue })
	vim.api.nvim_set_hl(0, "RainbowGreen", { fg = colors.green })
	vim.api.nvim_set_hl(0, "RainbowViolet", { fg = colors.purple })
	vim.api.nvim_set_hl(0, "RainbowCyan", { fg = colors.aqua })
end)

vim.g.rainbow_delimiters = { highlight = highlight }
require("ibl").setup({ scope = { show_start = false, show_end = false, highlight = highlight } })

require("lualine").setup({
	options = {
		theme = "catppuccin",
		component_separators = "|",
		section_separators = { left = "", right = "" },
	},
	sections = {
		lualine_a = {
			{ "mode", separator = { left = "█" }, right_padding = 2 },
		},
		lualine_b = { "filename", "branch" },
		lualine_c = { "fileformat" },
		lualine_x = {
			{
				require("noice").api.status.mode.get,
				cond = require("noice").api.status.mode.has,
				color = { fg = colors.gray },
			},
		},
		lualine_y = { "filetype", "progress" },
		lualine_z = {
			{ "location", separator = { right = "█" }, left_padding = 2 },
		},
	},
	inactive_sections = {
		lualine_a = { "filename" },
		lualine_b = {},
		lualine_c = {},
		lualine_x = {},
		lualine_y = {},
		lualine_z = { "location" },
	},
	extensions = {},
})

local tabby = function()
	--- https://github.com/nanozuki/tabby.nvim/blob/main/lua/tabby/presets.lua
	local util = require("tabby.util")

	local hl_tabline_fill = util.extract_nvim_hl("lualine_c_normal")
	local hl_tabline = util.extract_nvim_hl("lualine_b_normal")
	local hl_tabline_sel = util.extract_nvim_hl("lualine_a_normal")
	local hl_inactive = { fg = colors.crust, bg = colors.surface2 }

	local function tab_label(tabid, active)
		local icon = active and "" or ""
		local number = vim.api.nvim_tabpage_get_number(tabid)
		local name = util.get_tab_name(tabid)
		return string.format(" %s %d: %s ", icon, number, name)
	end

	local presets = {
		hl = "TabLineFill",
		layout = "tab_only",
		head = {
			{ " 󰣐 ", hl = { fg = hl_tabline.fg, bg = hl_tabline.bg } },
			{ "", hl = { fg = hl_tabline.bg, bg = hl_tabline_fill.bg } },
		},
		active_tab = {
			label = function(tabid)
				return {
					tab_label(tabid, true),
					hl = { fg = hl_tabline_sel.fg, bg = hl_tabline_sel.bg, style = "bold" },
				}
			end,
			left_sep = { " ", hl = { fg = hl_tabline_sel.bg, bg = hl_tabline_fill.bg } },
			right_sep = { "", hl = { fg = hl_tabline_sel.bg, bg = hl_tabline_fill.bg } },
		},
		inactive_tab = {
			label = function(tabid)
				return {
					tab_label(tabid, false),
					hl = hl_inactive,
				}
			end,
			left_sep = { " ", hl = { fg = hl_inactive.bg, bg = hl_tabline_fill.bg } },
			right_sep = { "", hl = { fg = hl_inactive.bg, bg = hl_tabline_fill.bg } },
		},
	}

	return presets
end

require("tabby").setup({
	tabline = tabby(),
})

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
