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
-- hi def NeogitDiffAddHighlight guibg=#404040 guifg=#859900
-- hi def NeogitDiffDeleteHighlight guibg=#404040 guifg=#dc322f
-- hi def NeogitDiffContextHighlight guibg=#333333 guifg=#b2b2b2
-- hi def NeogitDiffContext guibg=#262626 guifg=#b2b2b2
-- hi def NeogitHunkHeader guifg=#cccccc guibg=#404040
-- hi def NeogitHunkHeaderHighlight guifg=#cccccc guibg=#4d4d4d

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
       NeogitBranch = {fg = colors.bright_orange, bg = colors.dark0, bold = true},
       NeogitRemote = {fg = colors.bright_purple, bg = colors.dark0, bold = true},

       NeogitDiffAdd = {fg = colors.light0, bg = colors.faded_green },
       NeogitDiffDelete = {fg = colors.light0, bg = colors.faded_red },

       NeogitHunkHeader = {fg = colors.light0, bg = colors.dark2 },
       NeogitHunkHeaderHighlight = {fg = colors.light0, bg = colors.dark2 },

       NeogitUnstagedchanges = {fg = colors.bright_blue, bg = colors.dark0, bold = true},
       NeogitRecentcommits = {fg = colors.bright_blue, bg = colors.dark0, bold = true},

       NeogitObjectId = {fg = colors.neutral_yellow, bg = colors.dark0 },

       NeogitPopupSectionTitle = {fg = colors.bright_green, bg = colors.dark0, bold = true},
       NeogitPopupActionKey = {fg = colors.bright_orange, bg = colors.dark0 },
       NeogitPopupSwitchKey = {fg = colors.bright_orange, bg = colors.dark0 },
       NeogitPopupOptionKey = {fg = colors.bright_orange, bg = colors.dark0 },
       NeogitPopupActionDisabled = {fg = colors.dark4, bg = colors.dark0 },
    }
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
