-- vim.opt.background = "dark"
local colors = {
    dark0_hard = "#1d2021",
    dark0 = "#282828",
    dark0_soft = "#32302f",
    dark1 = "#3c3836",
    dark2 = "#504945",
    dark3 = "#665c54",
    dark4 = "#7c6f64",
    light0_hard = "#f9f5d7",
    light0 = "#fbf1c7",
    light0_soft = "#f2e5bc",
    light1 = "#ebdbb2",
    light2 = "#d5c4a1",
    light3 = "#bdae93",
    light4 = "#a89984",
    bright_red = "#fb4934",
    bright_green = "#b8bb26",
    bright_yellow = "#fabd2f",
    bright_blue = "#83a598",
    bright_purple = "#d3869b",
    bright_aqua = "#8ec07c",
    bright_orange = "#fe8019",
    neutral_red = "#cc241d",
    neutral_green = "#98971a",
    neutral_yellow = "#d79921",
    neutral_blue = "#458588",
    neutral_purple = "#b16286",
    neutral_aqua = "#689d6a",
    neutral_orange = "#d65d0e",
    faded_red = "#9d0006",
    faded_green = "#79740e",
    faded_yellow = "#b57614",
    faded_blue = "#076678",
    faded_purple = "#8f3f71",
    faded_aqua = "#427b58",
    faded_orange = "#af3a03",
    gray = "#928374",
}


require("gruvbox").setup({
    undercurl = false,
    underline = false,
    bold = false,
    italic = {
        strings = false,
        comments = false,
        operators = false,
        folds = false,
    },
    contrast = "hard",
    strikethrough = false,
    invert_selection = false,
    invert_signs = false,
    invert_tabline = false,
    invert_intend_guides = false,
    inverse = false, -- invert background for search, diffs, statuslines and errors
    overrides = {
        SignColumn = { bg = colors.dark0 },
        GruvboxRedSign = { bg = colors.dark0 },
        GruvboxOrangeSign = { bg = colors.dark0 },
        GruvboxYellowSign = { bg = colors.dark0 },
        GruvboxGreenSign = { bg = colors.dark0 },
        GruvboxAquaSign = { bg = colors.dark0 },
        GruvboxBlueSign = { bg = colors.dark0 },
        GruvboxPurpleSign = { bg = colors.dark0 },
        -- MiniFiles
        MiniFilesTitle = { fg = colors.light1, bg = colors.dark0_hard },
        MiniFilesTitleFocused = { fg = colors.neutral_green, bg = colors.dark0_hard },
        MiniFilesNormal = { fg = colors.light1, bg = colors.dark0_hard },
        MiniFilesBorder = { fg = colors.light1, bg = colors.dark0_hard },
        MiniFilesBorderModified = { fg = colors.bright_red, bg = colors.dark0_hard },
        -- NeoGit
        NeogitBranch = { fg = colors.bright_orange, bg = colors.dark0, bold = true },
        NeogitRemote = { fg = colors.bright_purple, bg = colors.dark0, bold = true },

        NeogitDiffAdd = { fg = colors.light0, bg = colors.faded_green },
        NeogitDiffDelete = { fg = colors.light0, bg = colors.faded_red },

        NeogitHunkHeader = { fg = colors.light0, bg = colors.dark2 },
        NeogitHunkHeaderHighlight = { fg = colors.light0, bg = colors.dark2 },

        NeogitUnstagedchanges = { fg = colors.bright_green, bg = colors.dark0, bold = true },
        NeogitRecentcommits = { fg = colors.bright_green, bg = colors.dark0, bold = true },
        NeogitUntrackedfiles = { fg = colors.bright_green, bg = colors.dark0, bold = true },
        NeogitUnmergedchanges = { fg = colors.bright_green, bg = colors.dark0, bold = true },
        NeogitStagedchanges = { fg = colors.bright_green, bg = colors.dark0, bold = true },
        NeogitStashes = { fg = colors.bright_green, bg = colors.dark0, bold = true },

        NeogitObjectId = { fg = colors.neutral_yellow, bg = colors.dark0 },
        NeogitStash = { fg = colors.neutral_yellow, bg = colors.dark0 },

        NeogitPopupSectionTitle = { fg = colors.bright_green, bg = colors.dark0, bold = true },
        NeogitPopupActionKey = { fg = colors.bright_orange, bg = colors.dark0 },
        NeogitPopupSwitchKey = { fg = colors.bright_orange, bg = colors.dark0 },
        NeogitPopupOptionKey = { fg = colors.bright_orange, bg = colors.dark0 },
        NeogitPopupActionDisabled = { fg = colors.dark4, bg = colors.dark0 },

        -- for noice
        NoiceCmdlinePopup = { bg = colors.dark0_hard },
        NoiceCmdlinePopupBorder = { bg = colors.dark0_hard, fg = colors.bright_blue },
        NoiceCmdlinePopupPrompt = { bg = colors.dark0_hard, fg = colors.bright_blue },
        NoiceCmdlinePopupTitle = { bg = colors.dark0_hard, fg = colors.bright_blue },
        NoiceCmdlineIcon = { bg = colors.dark0, fg = colors.bright_green },

        NoiceConfirmBorder = { bg = colors.dark0, fg = colors.bright_blue },
        NoiceFormatConfirmDefault = { bg = colors.faded_green, fg = colors.light0 },

        -- Tabby HL
        TabLineFill = { bg = colors.dark0_hard, fg = colors.light0 },
        TabLine = { bg = colors.dark0_soft, fg = colors.gray },
        TabLineSel = { bg = colors.dark3, fg = colors.light0 },
        TabLineEdge = { bg = colors.bright_purple, fg = colors.dark0 },

        -- TSContext
        NormalFloat = { bg = colors.dark0 },

        -- completion menu
        PmenuSel = { bg = colors.grey, fg = "NONE" },
        Pmenu = { fg = colors.light0, bg = colors.dark0 },

        CmpItemAbbrDeprecated = { fg = colors.dark3, bg = "NONE", strikethrough = false },
        CmpItemAbbrMatch = { fg = colors.bright_blue, bg = "NONE", bold = true },
        CmpItemAbbrMatchFuzzy = { fg = colors.bright_blue, bg = "NONE", bold = true },
        CmpItemMenu = { fg = colors.bright_purple, bg = "NONE", italic = false },

        CmpItemKindField = { fg = colors.light0_soft, bg = colors.neutral_red },
        CmpItemKindProperty = { fg = colors.light0_soft, bg = colors.neutral_red },
        CmpItemKindEvent = { fg = colors.light0_soft, bg = colors.neutral_red },

        CmpItemKindText = { fg = colors.light0_soft, bg = colors.neutral_green },
        CmpItemKindEnum = { fg = colors.light0_soft, bg = colors.neutral_green },
        CmpItemKindKeyword = { fg = colors.light0_soft, bg = colors.neutral_green },

        CmpItemKindConstant = { fg = colors.light0_soft, bg = colors.neutral_yellow },
        CmpItemKindConstructor = { fg = colors.light0_soft, bg = colors.neutral_yellow },
        CmpItemKindReference = { fg = colors.light0_soft, bg = colors.neutral_yellow },

        CmpItemKindFunction = { fg = colors.light0_soft, bg = colors.neutral_purple },
        CmpItemKindStruct = { fg = colors.light0_soft, bg = colors.neutral_purple },
        CmpItemKindClass = { fg = colors.light0_soft, bg = colors.neutral_purple },
        CmpItemKindModule = { fg = colors.light0_soft, bg = colors.neutral_purple },
        CmpItemKindOperator = { fg = colors.light0_soft, bg = colors.neutral_purple },

        CmpItemKindVariable = { fg = colors.light0_soft, bg = colors.gray },
        CmpItemKindFile = { fg = colors.light0_soft, bg = colors.gray },

        CmpItemKindUnit = { fg = colors.light0_soft, bg = colors.neutral_orange },
        CmpItemKindSnippet = { fg = colors.light0_soft, bg = colors.neutral_orange },
        CmpItemKindFolder = { fg = colors.light0_soft, bg = colors.neutral_orange },

        CmpItemKindMethod = { fg = colors.light0_soft, bg = colors.neutral_blue },
        CmpItemKindValue = { fg = colors.light0_soft, bg = colors.neutral_blue },
        CmpItemKindEnumMember = { fg = colors.light0_soft, bg = colors.neutral_blue },

        CmpItemKindInterface = { fg = colors.light0_soft, bg = colors.neutral_aqua },
        CmpItemKindColor = { fg = colors.light0_soft, bg = colors.neutral_aqua },
        CmpItemKindTypeParameter = { fg = colors.light0_soft, bg = colors.neutral_aqua },

    }
})
vim.opt.termguicolors = true
vim.cmd('colorscheme gruvbox')

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
local hooks = require "ibl.hooks"
-- create the highlight groups in the highlight setup hook, so they are reset
-- every time the colorscheme changes
hooks.register(hooks.type.HIGHLIGHT_SETUP, function()
    vim.api.nvim_set_hl(0, "RainbowRed", { fg = colors.bright_red })
    vim.api.nvim_set_hl(0, "RainbowYellow", { fg = colors.bright_yellow })
    vim.api.nvim_set_hl(0, "RainbowBlue", { fg = colors.bright_blue })
    vim.api.nvim_set_hl(0, "RainbowOrange", { fg = colors.bright_orange })
    vim.api.nvim_set_hl(0, "RainbowGreen", { fg = colors.bright_green })
    vim.api.nvim_set_hl(0, "RainbowViolet", { fg = colors.bright_purple })
    vim.api.nvim_set_hl(0, "RainbowCyan", { fg = colors.bright_aqua })
end)

vim.g.rainbow_delimiters = { highlight = highlight }
require("ibl").setup { scope = { show_start = false, show_end = false, highlight = highlight } }

hooks.register(hooks.type.SCOPE_HIGHLIGHT, hooks.builtin.scope_highlight_from_extmark)

-- https://github.com/nvim-lualine/lualine.nvim/blob/master/examples/bubbles.lua
-- Bubbles config for lualine
-- Author: lokesh-krishna
-- MIT license, see LICENSE for more details.

colors.blue         = colors.neutral_blue
colors.cyan         = colors.neutral_aqua
colors.black        = colors.dark0_hard
colors.white        = colors.light1
colors.red          = colors.bright_red
colors.violet       = colors.neutral_purple
colors.grey         = colors.dark0_soft
colors.green        = colors.neutral_green
colors.orange       = colors.neutral_orange

local bubbles_theme = {
    normal = {
        a = { fg = colors.black, bg = colors.blue },
        b = { fg = colors.white, bg = colors.grey },
        c = { fg = colors.gray, bg = colors.black },
    },

    insert = { a = { fg = colors.black, bg = colors.green } },
    command = { a = { fg = colors.black, bg = colors.orange } },
    visual = { a = { fg = colors.black, bg = colors.violet } },
    replace = { a = { fg = colors.black, bg = colors.red } },

    inactive = {
        a = { fg = colors.white, bg = colors.black },
        b = { fg = colors.white, bg = colors.black },
        c = { fg = colors.black, bg = colors.black },
    },
}

require('lualine').setup({
    options = {
        theme = bubbles_theme,
        component_separators = '|',
        section_separators = { left = '', right = '' },
    },
    sections = {
        lualine_a = {
            { 'mode', separator = { left = '█' }, right_padding = 2 },
        },
        lualine_b = { 'filename', 'branch' },
        lualine_c = { 'fileformat' },
        lualine_x = {
            {
                require("noice").api.status.mode.get,
                cond = require("noice").api.status.mode.has,
                color = { fg = colors.gray },
            }
        },
        lualine_y = { 'filetype', 'progress' },
        lualine_z = {
            { 'location', separator = { right = '█' }, left_padding = 2 },
        },
    },
    inactive_sections = {
        lualine_a = { 'filename' },
        lualine_b = {},
        lualine_c = {},
        lualine_x = {},
        lualine_y = {},
        lualine_z = { 'location' },
    },
    extensions = {},
})

local tabby = function()
    --- https://github.com/nanozuki/tabby.nvim/blob/main/lua/tabby/presets.lua
    local util = require('tabby.util')

    local hl_tabline_fill = util.extract_nvim_hl('lualine_c_normal')
    local hl_tabline = util.extract_nvim_hl('lualine_b_normal')
    local hl_tabline_sel = util.extract_nvim_hl('lualine_a_normal')
    local hl_inactive = { fg = colors.light0, bg = colors.dark1 }

    local function tab_label(tabid, active)
        local icon = active and '' or ''
        local number = vim.api.nvim_tabpage_get_number(tabid)
        local name = util.get_tab_name(tabid)
        return string.format(' %s %d: %s ', icon, number, name)
    end

    local presets = {
        hl = 'lualine_c_normal',
        layout = 'tab_only',
        head = {
            { ' 󰣐 ', hl = { fg = hl_tabline.fg, bg = hl_tabline.bg } },
            { '', hl = { fg = hl_tabline.bg, bg = hl_tabline_fill.bg } },
        },
        active_tab = {
            label = function(tabid)
                return {
                    tab_label(tabid, true),
                    hl = { fg = hl_tabline_sel.fg, bg = hl_tabline_sel.bg, style = 'bold' },
                }
            end,
            left_sep = { ' ', hl = { fg = hl_tabline_sel.bg, bg = hl_tabline_fill.bg } },
            right_sep = { '', hl = { fg = hl_tabline_sel.bg, bg = hl_tabline_fill.bg } },
        },
        inactive_tab = {
            label = function(tabid)
                return {
                    tab_label(tabid, false),
                    hl = hl_inactive,
                }
            end,
            left_sep = { ' ', hl = { fg = hl_inactive.bg, bg = hl_tabline_fill.bg } },
            right_sep = { '', hl = { fg = hl_inactive.bg, bg = hl_tabline_fill.bg } },
        },
    }

    return presets
end

require("tabby").setup({
	tabline = tabby(),
})
