return {
    {
        'smoka7/hop.nvim',
        version = "*",
        opts = {
            keys = "arstgmneioqwfpbjluyxcdvzkh"
        },
    },
    {
        'mizlan/iswap.nvim',
        event = "VeryLazy",
        opts = {
            keys = "arstgmneioqwfpbjluyxcdvzkh"
        }
    },
    {
        'mizlan/iswap.nvim',
        event = "VeryLazy",
        opts = {
            keys = "arstgmneioqwfpbjluyxcdvzkh"
        }
    },
    'monaqa/dial.nvim',
    'tpope/vim-sensible',
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
        'AckslD/nvim-neoclip.lua',
        dependencies = {
            'nvim-telescope/telescope.nvim',
            'kkharji/sqlite.lua',
        },
        config = function()
            require('neoclip').setup {
                enable_persistent_history = true,
                default_register = { '"', '+' }
            }
        end,
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
    'anuvyklack/hydra.nvim',
    'kevinhwang91/nvim-bqf',

    -- Buffer/Window Management
    {
        'tiagovla/scope.nvim',
        config = function()
            require('scope').setup {}
        end
    },
    'famiu/bufdelete.nvim',
    'mrjones2014/smart-splits.nvim',
    'sindrets/winshift.nvim',
    {
        's1n7ax/nvim-window-picker',
        name = 'window-picker',
        event = 'VeryLazy',
        version = '2.*',
        config = function()
            require 'window-picker'.setup()
        end,
    },
    {
        'numToStr/Navigator.nvim',
        config = function()
            require('Navigator').setup()
        end
    },
    -- sessions
    {
        "folke/persistence.nvim",
        event = "BufReadPre",
        opts = {
            options = { "buffers", "curdir", "tabpages", "winsize", "globals" },
        }
    },
}
