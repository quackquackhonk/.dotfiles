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

require("catppuccin").setup({
	flavour = "mocha",
	show_end_of_buffer = true,
	no_italic = true,
	no_underline = true,
	custom_highlights = function(colors)
		return {
			TabLine = { bg = colors.mantle },
		}
	end,
})
vim.cmd("colorscheme catppuccin-mocha")

local colors = require("catppuccin.palettes").get_palette("mocha")

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
