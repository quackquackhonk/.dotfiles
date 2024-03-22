local p = {
	rosewater = "#f5e0dc",
	flamingo = "#f2cdcd",
	pink = "#f5c2e7",
	mauve = "#cba6f7",
	red = "#f38ba8",
	maroon = "#eba0ac",
	peach = "#fab387",
	yellow = "#f9e2af",
	green = "#a6e3a1",
	teal = "#94e2d5",
	sky = "#89dceb",
	sapphire = "#74c7ec",
	blue = "#89b4fa",
	lavender = "#b4befe",
	text = "#cdd6f4",
	subtext1 = "#bac2de",
	subtext0 = "#a6adc8",
	overlay2 = "#9399b2",
	overlay1 = "#7f849c",
	overlay0 = "#6c7086",
	surface2 = "#585b70",
	surface1 = "#45475a",
	surface0 = "#313244",
	base = "#1e1e2e",
	mantle = "#181825",
	crust = "#11111b",
}

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
	show_end_of_buffer = true,
	no_italic = true,
	no_underline = true,
	custom_highlights = function(colors)
		return {
			TabLine = { bg = colors.mantle },

			-- completion menu
			PmenuSel = { bg = colors.mantle, fg = "NONE" },
			Pmenu = { fg = colors.white, bg = colors.black },

			CmpItemAbbrDeprecated = { fg = colors.grey, bg = "NONE", strikethrough = false },
			CmpItemAbbrMatch = { fg = colors.sapphire, bg = "NONE", bold = false },
			CmpItemAbbrMatchFuzzy = { fg = colors.sapphire, bg = "NONE", bold = false },
			CmpItemMenu = { fg = colors.pink, bg = "NONE", italic = false },

			CmpItemKindField = { fg = colors.base, bg = colors.red },
			CmpItemKindProperty = { fg = colors.base, bg = colors.red },
			CmpItemKindEvent = { fg = colors.base, bg = colors.red },

			CmpItemKindText = { fg = colors.base, bg = colors.green },
			CmpItemKindEnum = { fg = colors.base, bg = colors.green },
			CmpItemKindKeyword = { fg = colors.base, bg = colors.green },

			CmpItemKindConstant = { fg = colors.base, bg = colors.yellow },
			CmpItemKindConstructor = { fg = colors.base, bg = colors.yellow },
			CmpItemKindReference = { fg = colors.base, bg = colors.yellow },

			CmpItemKindFunction = { fg = colors.base, bg = colors.purple },
			CmpItemKindStruct = { fg = colors.base, bg = colors.purple },
			CmpItemKindClass = { fg = colors.base, bg = colors.purple },
			CmpItemKindModule = { fg = colors.base, bg = colors.purple },
			CmpItemKindOperator = { fg = colors.base, bg = colors.purple },

			CmpItemKindVariable = { fg = colors.base, bg = colors.pink },
			CmpItemKindFile = { fg = colors.base, bg = colors.pink },

			CmpItemKindUnit = { fg = colors.base, bg = colors.orange },
			CmpItemKindSnippet = { fg = colors.base, bg = colors.orange },
			CmpItemKindFolder = { fg = colors.base, bg = colors.orange },

			CmpItemKindMethod = { fg = colors.base, bg = colors.blue },
			CmpItemKindValue = { fg = colors.base, bg = colors.blue },
			CmpItemKindEnumMember = { fg = colors.base, bg = colors.blue },

			CmpItemKindInterface = { fg = colors.base, bg = colors.aqua },
			CmpItemKindColor = { fg = colors.base, bg = colors.aqua },
			CmpItemKindTypeParameter = { fg = colors.base, bg = colors.aqua },
		}
	end,
	integrations = {
		hop = true,
		mason = true,
		mini = {
			enabled = true,
		},
		neogit = true,
		neotest = true,
		noice = true,
		cmp = true,
		native_lsp = {
			enabled = true,
			virtual_text = {
				-- errors = { "italic" },
				-- hints = { "italic" },
				-- warnings = { "italic" },
				-- information = { "italic" },
				errors = {},
				hints = {},
				warnings = {},
				information = {},
			},
			underlines = {
				-- errors = { "underline" },
				-- hints = { "underline" },
				-- warnings = { "underline" },
				-- information = { "underline" },
				errors = {},
				hints = {},
				warnings = {},
				information = {},
			},
			inlay_hints = {
				background = true,
			},
		},
        which_key = true,
	},
})
vim.cmd("colorscheme catppuccin-mocha")

-- Rainbow Delimiters / indent blankline setup
local highlight = {
	"RainbowRed",
	"RainbowYellow",
	"RainbowBlue",
	"RainbowOrange",
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
	vim.api.nvim_set_hl(0, "RainbowOrange", { fg = colors.orange })
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
		hl = "TabLine",
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
