-- install lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
    'monaqa/dial.nvim',
    'tpope/vim-sensible',
    'tpope/vim-surround',
    {
        'numToStr/Comment.nvim',
        config = function()
            require('Comment').setup()
        end
    },
    {
        'windwp/nvim-autopairs',
        config = function() require("nvim-autopairs").setup {} end
    },
    {
        'preservim/vim-markdown',
        config = function()
            vim.g.vim_markdown_folding_disabled = 1
        end
    },
    {
        'nvim-orgmode/orgmode',
        config = function ()
            require('orgmode').setup_ts_grammar()
            require('nvim-treesitter.configs').setup {
                -- If TS highlights are not enabled at all, or disabled via `disable` prop,
                -- highlighting will fallback to default Vim syntax highlighting
                highlight = {
                    enable = true,
                    -- Required for spellcheck, some LaTex highlights and
                    -- code block highlights that do not have ts grammar
                    additional_vim_regex_highlighting = {'org'},
                },
                ensure_installed = {'org'}, -- Or run :TSUpdate org
            }
            require('orgmode').setup({
                org_agenda_files = {'~/org/**/*'},
                org_default_notes_file = '~/org/refile.org',
            })
        end,
        dependencies = 'nvim-treesitter/nvim-treesitter',
    },
    -- keymap
    {
        'folke/which-key.nvim',
        config = function()
            require('which-key').setup({})
        end
    },
    {
        'sudormrfbin/cheatsheet.nvim',
        config = function()
            require("cheatsheet").setup({})
        end
    },
    {
        'phaazon/hop.nvim',
        branch = 'v2',
        config = function()
            require 'hop'.setup { keys = 'ntesiroamglpufywqjb' }
        end
    },
    'anuvyklack/hydra.nvim',
    'kevinhwang91/nvim-bqf',

    -- Buffer/Window Management
    {
        'romgrk/barbar.nvim',
        config = function()
            require('bufferline').setup {
                auto_hide = false,
            }
        end,
    },
    'mrjones2014/smart-splits.nvim',
    'sindrets/winshift.nvim',
    'szw/vim-maximizer',
    -- Git Integration
    'tpope/vim-fugitive',
    {
        'lewis6991/gitsigns.nvim',
        config = function()
            require('gitsigns').setup {}
        end
    },
    {
        'ahmedkhalf/project.nvim',
        config = function()
            require('project_nvim').setup({})
        end,
    },

    -- LSP / Completion / Treesitter
    {
        'mfussenegger/nvim-dap',
        dependencies = {
            'nvim-telescope/telescope-dap.nvim',
            'rcarriga/nvim-dap-ui',
            'theHamsta/nvim-dap-virtual-text',
            'jbyuki/one-small-step-for-vimkind',
        }
    },
    {
        'nvim-telescope/telescope.nvim',
        branch = '0.1.x',
        config = function()
            require('plugins.telescope')
        end
    },
    {
        'nvim-telescope/telescope-fzf-native.nvim',
        run = 'make'
    },
    {
        "nvim-telescope/telescope-frecency.nvim",
        dependencies = {"kkharji/sqlite.lua"}
    },
    {
        'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate'
    },
    {
        'stevearc/oil.nvim',
        opts = {},
        -- Optional dependencies
        dependencies = { "nvim-tree/nvim-web-devicons" },
    },
    'neovim/nvim-lspconfig',
    {
        'williamboman/mason.nvim',
        config = function()
            require('mason').setup()
        end
    },
    {
        'williamboman/mason-lspconfig.nvim',
        config = function()
            require('mason-lspconfig').setup()
        end
    },
    'hrsh7th/nvim-cmp',
    'hrsh7th/cmp-nvim-lsp',
    'hrsh7th/cmp-buffer',
    'hrsh7th/cmp-path',
    'L3MON4D3/LuaSnip',
    'saadparwaiz1/cmp_luasnip',
    'rafamadriz/friendly-snippets',
    'onsails/lspkind.nvim',
    'simrat39/rust-tools.nvim',
    {
        'simrat39/symbols-outline.nvim',
        config = function()
            require("symbols-outline").setup({
                position = 'left',
            })
        end
    },
    {
        'jose-elias-alvarez/null-ls.nvim',
        config = function()
            local nullls = require("null-ls")
            nullls.setup({
                sources = {
                    nullls.builtins.formatting.black,
                    nullls.builtins.formatting.clang_format,
                }
            })
        end
    },
    'mfussenegger/nvim-jdtls',

    -- Theme / UI
    {
        'glepnir/dashboard-nvim',
        event = 'VimEnter',
        config = function()
            require('dashboard').setup {
                -- config
            }
        end,
        dependencies = { {'nvim-tree/nvim-web-devicons'}}
    },
    'nvim-lua/popup.nvim',
    'nvim-lua/plenary.nvim',
    {
        'kyazdani42/nvim-tree.lua',
        config = function()
            require("nvim-tree").setup()
        end
    },
    {
        'stevearc/dressing.nvim',
        config = function()
            require('dressing').setup({
                input = {
                    default_prompt = ">",
                },
                select = {
                    telescope = require('telescope.themes').get_cursor({})
                },
            })
        end
    },
    'ellisonleao/gruvbox.nvim',
    'folke/lsp-colors.nvim',
    {
        'nvim-lualine/lualine.nvim',
        config = function()
            require('lualine').setup {
                options = {
                    theme = 'gruvbox'
                }
            }
        end
    },
    {
        'folke/trouble.nvim',
        config = function()
            require("trouble").setup {}
        end
    },
    {
        'norcalli/nvim-colorizer.lua',
        config = function()
            require 'colorizer'.setup()
        end
    },
})

vim.g.mapleader = " "

