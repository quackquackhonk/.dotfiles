return {
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
        "nvim-neorg/neorg",
        build = ":Neorg sync-parsers",
        dependencies = { "nvim-lua/plenary.nvim" },
        config = function()
            require("neorg").setup {
                load = {
                    ["core.defaults"] = {},  -- Loads default behaviour
                    ["core.concealer"] = {}, -- Adds pretty icons to your documents
                    ["core.dirman"] = {      -- Manages Neorg workspaces
                        config = {
                            workspaces = {
                                notes = "~/Dropbox/",
                            },
                        },
                    },
                    ["core.keybinds"] = {
                        config = {
                            default_keybinds = true,
                        }
                    }
                },
            }
        end,
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
    'famiu/bufdelete.nvim',
    'mrjones2014/smart-splits.nvim',
    'sindrets/winshift.nvim',
    'szw/vim-maximizer',
    {
        's1n7ax/nvim-window-picker',
        name = 'window-picker',
        event = 'VeryLazy',
        version = '2.*',
        config = function()
            require 'window-picker'.setup()
        end,
    },
}