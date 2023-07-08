-- vim.opt.background = "dark"
--[[  dark0_hard = "#1d2021",
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
  gray = "#928374", ]]

local colors = require("gruvbox.palette")
require("gruvbox").setup({
    undercurl = true,
    underline = true,
    bold = true,
    italic = {
        strings = false,
        comments = false,
        operators = false,
        folds = false,
    },
    strikethrough = true,
    invert_selection = false,
    invert_signs = false,
    invert_tabline = false,
    invert_intend_guides = false,
    inverse = true, -- invert background for search, diffs, statuslines and errors
    overrides = {
        -- Telescope Overrides
        TelescopeNormal = {
            bg = colors.dark0_hard,
            fg = colors.light1,
        },
        TelescopePromptNormal = {
            fg = colors.light1,
            bg = colors.dark0_hard,
        },
        TelescopeResultsNormal = {
            bg = colors.dark0_hard,
            fg = colors.light1,
        },
        TelescopePreviewNormal = {
            bg = colors.dark0_hard,
            fg = colors.light1,
        },
        TelescopeBorder = {
            fg = colors.light1,
            bg = colors.dark0_hard,
        },
        TelescopePromptBorder = {
            fg = colors.light1,
            bg = colors.dark0_hard,
        },
        TelescopePromptPrefix = {
            fg = colors.neutral_red,
            bg = colors.dark0_hard,
        },
        -- Title
        TelescopePreviewTitle = {
            fg = colors.light1,
            bg = colors.dark0_hard,
        },
        TelescopePromptTitle = {
            fg = colors.light1,
            bg = colors.dark0_hard,
        },
        TelescopeResultsTitle = {
            fg = colors.light1,
            bg = colors.dark0_hard,
        },
        -- Trouble overrides
        TroubleNormal = {
            fg = colors.light1,
            bg = colors.dark0_hard,
        },
        TroubleCount = {
            fg = colors.neutral_green,
            bg = colors.dark0_hard,
        },
        TroubleFoldIcon = {
            fg = colors.bright_yellow,
            bg = colors.dark0_hard,
        },
        TroubleSignError = {
            fg = colors.bright_red,
            bg = colors.dark0_hard,
        },
        TroubleSignWarning = {
            fg = colors.bright_yellow,
            bg = colors.dark0_hard,
        },
        TroubleSignHint = {
            fg = colors.bright_aqua,
            bg = colors.dark0_hard,
        },
        TelescopeSelection = { bg = colors.dark0_soft, fg = colors.light0 },
        TelescopeResultsDiffAdd = { fg = colors.faded_green },
        TelescopeResultsDiffChange = { fg = colors.faded_yellow },
        TelescopeResultsDiffDelete = { fg = colors.faded_purple },
        -- CMP menu overrides
        CmpItemAbbr = { fg = colors.light0, bg = "NONE" },
        CmpItemMenu = { fg = colors.neutral_purple, bg = "NONE" },
        CmpItemKindText = { fg = colors.light1, bg = colors.neutral_orange },
        CmpItemKindMethod = { fg = colors.light1, bg = colors.neutral_blue },
        CmpItemKindFunction = { fg = colors.light1, bg = colors.neutral_blue },
        CmpItemKindConstructor = { fg = colors.light1, bg = colors.neutral_yellow },
        CmpItemKindField = { fg = colors.light1, bg = colors.neutral_blue },
        CmpItemKindClass = { fg = colors.light1, bg = colors.neutral_yellow },
        CmpItemKindInterface = { fg = colors.light1, bg = colors.neutral_yellow },
        CmpItemKindModule = { fg = colors.light1, bg = colors.neutral_blue },
        CmpItemKindProperty = { fg = colors.light1, bg = colors.neutral_blue },
        CmpItemKindValue = { fg = colors.light1, bg = colors.neutral_orange },
        CmpItemKindEnum = { fg = colors.light1, bg = colors.neutral_yellow },
        CmpItemKindKeyword = { fg = colors.light1, bg = colors.neutral_purple },
        CmpItemKindSnippet = { fg = colors.light1, bg = colors.neutral_green },
        CmpItemKindFile = { fg = colors.light1, bg = colors.neutral_blue },
        CmpItemKindEnumMember = { fg = colors.light1, bg = colors.neutral_aqua },
        CmpItemKindConstant = { fg = colors.light1, bg = colors.neutral_orange },
        CmpItemKindStruct = { fg = colors.light1, bg = colors.neutral_yellow },
        CmpItemKindTypeParameter = { fg = colors.light1, bg = colors.neutral_yellow },
    },
})
vim.opt.termguicolors = true
vim.cmd('colorscheme gruvbox')


-- Credit: glepnir
local lualine = require('lualine')

-- Color table for highlights
lualine.setup {
    options = {
        theme = 'gruvbox'
    }
}
