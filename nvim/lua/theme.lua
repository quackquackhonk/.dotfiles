vim.opt.background="dark"
local colors = require("gruvbox.palette")
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

require("gruvbox").setup({
  undercurl = true,
  underline = true,
  bold = true,
  italic = true,
  strikethrough = true,
  invert_selection = false,
  invert_signs = false,
  invert_tabline = false,
  invert_intend_guides = false,
  inverse = true, -- invert background for search, diffs, statuslines and errors
  contrast = "", -- can be "hard", "soft" or empty string
  overrides = {
    -- Telescope Overrides
    TelescopeNormal = {
        bg = colors.dark0_hard,
        fg = colors.dark0_hard,
    },
    TelescopePromptNormal = {
      fg = colors.light0,
      bg = colors.dark0_soft,
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
        fg = colors.dark0_hard,
        bg = colors.dark0_hard,
    },
    TelescopePromptBorder = {
      fg = colors.dark0_soft,
      bg = colors.dark0_soft,
    },
    TelescopePromptPrefix = {
      fg = colors.faded_purple,
      bg = colors.dark0_soft,
    },
    -- Title
    TelescopePreviewTitle = {
      fg = colors.dark0,
      bg = colors.neutral_blue,
    },
    TelescopePromptTitle = {
      fg = colors.dark0,
      bg = colors.faded_purple,
    },
    TelescopeResultsTitle = {
      fg = colors.dark0_hard,
      bg = colors.dark0_hard,
    },
    TelescopeSelection = { bg = colors.dark0_soft , fg = colors.light0 },
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
    CmpItemKindValue = { fg = colors.light1, bg = colors.neutral_orange},
    CmpItemKindEnum = { fg = colors.light1, bg = colors.neutral_yellow },
    CmpItemKindKeyword = { fg = colors.light1, bg = colors.neutral_purple },
    CmpItemKindSnippet = { fg = colors.light1, bg = colors.neutral_green },
    CmpItemKindFile = { fg = colors.light1, bg = colors.neutral_blue },
    CmpItemKindEnumMember = { fg = colors.light1, bg = colors.neutral_aqua },
    CmpItemKindConstant = { fg = colors.light1, bg = colors.neutral_orange},
    CmpItemKindStruct = { fg = colors.light1, bg = colors.neutral_yellow },
    CmpItemKindTypeParameter = { fg = colors.light1, bg = colors.neutral_yellow },
  },
})
vim.cmd('colorscheme gruvbox')

-- LUA LINE CONFIG

-- Eviline config for lualine
-- Author: shadmansaleh
-- Credit: glepnir
local lualine = require('lualine')

-- Color table for highlights
-- stylua: ignore
local ll_colors = {
  bg       = colors.dark1,
  fg       = colors.light1,
  yellow   = colors.bright_yellow,
  cyan     = colors.bright_blue,
  darkblue = colors.faded_blue,
  green    = colors.bright_green,
  orange   = colors.bright_orange,
  violet   = colors.neutral_purple,
  magenta  = colors.bright_purple,
  blue     = colors.neutral_blue,
  red      = colors.bright_red,
}

local conditions = {
  buffer_not_empty = function()
    return vim.fn.empty(vim.fn.expand('%:t')) ~= 1
  end,
  hide_in_width = function()
    return vim.fn.winwidth(0) > 80
  end,
  check_git_workspace = function()
    local filepath = vim.fn.expand('%:p:h')
    local gitdir = vim.fn.finddir('.git', filepath .. ';')
    return gitdir and #gitdir > 0 and #gitdir < #filepath
  end,
}

-- Config
local config = {
  options = {
    -- Disable sections and component separators
    component_separators = '',
    section_separators = '',
    theme = {
      -- We are going to use lualine_c an lualine_x as left and
      -- right section. Both are highlighted by c theme .  So we
      -- are just setting default looks o statusline
      normal = { c = { fg = ll_colors.fg, bg = ll_colors.bg } },
      inactive = { c = { fg = ll_colors.fg, bg = ll_colors.bg } },
    },
  },
  sections = {
    -- these are to remove the defaults
    lualine_a = {},
    lualine_b = {},
    lualine_y = {},
    lualine_z = {},
    -- These will be filled later
    lualine_c = {},
    lualine_x = {},
  },
  inactive_sections = {
    -- these are to remove the defaults
    lualine_a = {},
    lualine_b = {},
    lualine_y = {},
    lualine_z = {},
    lualine_c = {},
    lualine_x = {},
  },
}

-- Inserts a component in lualine_c at left section
local function ins_left(component)
  table.insert(config.sections.lualine_c, component)
end

-- Inserts a component in lualine_x ot right section
local function ins_right(component)
  table.insert(config.sections.lualine_x, component)
end

ins_left {
  function()
    return '▊'
  end,
  color = { fg = ll_colors.violet }, -- Sets highlighting of component
  padding = { left = 0, right = 1 }, -- We don't need space before this
}

ins_left {
  -- mode component
  function()
    return ''
  end,
  color = function()
    -- auto change color according to neovims mode
    local mode_color = {
      n = ll_colors.red,
      i = ll_colors.green,
      v = ll_colors.blue,
      -- [''] = colors.blue,
      V = ll_colors.blue,
      c = ll_colors.magenta,
      no = ll_colors.red,
      s = ll_colors.orange,
      S = ll_colors.orange,
      -- [''] = colors.orange,
      ic = ll_colors.yellow,
      R = ll_colors.violet,
      Rv = ll_colors.violet,
      cv = ll_colors.red,
      ce = ll_colors.red,
      r = ll_colors.cyan,
      rm = ll_colors.cyan,
      ['r?'] = ll_colors.cyan,
      ['!'] = ll_colors.red,
      t = ll_colors.red,
    }
    return { fg = mode_color[vim.fn.mode()] }
  end,
  padding = { right = 1 },
}

ins_left {
  -- filesize component
  'filesize',
  cond = conditions.buffer_not_empty,
}

ins_left {
  'filename',
  cond = conditions.buffer_not_empty,
  color = { fg = ll_colors.magenta, gui = 'bold' },
}

ins_left { 'location' }

ins_left { 'progress', color = { fg = ll_colors.fg, gui = 'bold' } }

ins_left {
  'diagnostics',
  sources = { 'nvim_diagnostic' },
  symbols = { error = ' ', warn = ' ', info = ' ' },
  diagnostics_color = {
    color_error = { fg = ll_colors.red },
    color_warn = { fg = ll_colors.yellow },
    color_info = { fg = ll_colors.cyan },
  },
}

-- Insert mid section. You can make any number of sections in neovim :)
-- for lualine it's any number greater then 2
ins_left {
  function()
    return '%='
  end,
}

ins_left {
  -- Lsp server name .
  function()
    local msg = 'No Active Lsp'
    local buf_ft = vim.api.nvim_buf_get_option(0, 'filetype')
    local clients = vim.lsp.get_active_clients()
    if next(clients) == nil then
      return msg
    end
    for _, client in ipairs(clients) do
      local filetypes = client.config.filetypes
      if filetypes and vim.fn.index(filetypes, buf_ft) ~= -1 then
        return client.name
      end
    end
    return msg
  end,
  icon = ' LSP:',
  color = { fg = colors.fg, gui = 'bold' },
}

-- Add components to right sections
ins_right {
  'o:encoding', -- option component same as &encoding in viml
  fmt = string.upper, -- I'm not sure why it's upper case either ;)
  cond = conditions.hide_in_width,
  color = { fg = ll_colors.green, gui = 'bold' },
}

ins_right {
  'fileformat',
  fmt = string.upper,
  icons_enabled = false, -- I think icons are cool but Eviline doesn't have them. sigh
  color = { fg = ll_colors.green, gui = 'bold' },
}

ins_right {
  'branch',
  icon = '',
  color = { fg = ll_colors.violet, gui = 'bold' },
}

ins_right {
  'diff',
  -- Is it me or the symbol for modified us really weird
  symbols = { added = ' ', modified = '柳 ', removed = ' ' },
  diff_color = {
    added = { fg = ll_colors.green },
    modified = { fg = ll_colors.orange },
    removed = { fg = ll_colors.red },
  },
  cond = conditions.hide_in_width,
}

ins_right {
  function()
    return '▊'
  end,
  color = { fg = ll_colors.violet },
  padding = { left = 1 },
}

-- Now don't forget to initialize lualine
lualine.setup(config)
