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
        "NeogitOrg/neogit",
        dependencies = {
            "nvim-lua/plenary.nvim",         -- required
            "sindrets/diffview.nvim",        -- optional - Diff integration
            -- Only one of these is needed, not both.
            "nvim-telescope/telescope.nvim", -- optional
        },
        opts = {
            kind = "split"
        }
    },
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
        'Shatur/neovim-tasks',
        dependencies = {
            'nvim-lua/plenary.nvim',
            'mfussenegger/nvim-dap',
        },
        config = function()
            local Path = require('plenary.path')
            require('tasks').setup({})
        end,
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
        dependencies = { "kkharji/sqlite.lua" }
    },
    {
        'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate'
    },
    {
        'echasnovski/mini.files',
        version = false,
        config = function()
            require('mini.files').setup({
                mappings = {
                    close       = 'q',
                    go_in       = '<Return>',
                    go_in_plus  = '<C-Return>',
                    go_out      = '<BS>',
                    go_out_plus = '<C-BS>',
                    reset       = '<C-r>',
                    reveal_cwd  = '@',
                    show_help   = 'g?',
                    synchronize = '=',
                    trim_left   = '<',
                    trim_right  = '>',
                },
            })
        end
    },
    'neovim/nvim-lspconfig',
    {
        'williamboman/mason.nvim',
        config = function()
            require('mason').setup()
        end
    },
    {
        "NoahTheDuke/vim-just",
        event = { "BufReadPre", "BufNewFile" },
        ft = { "\\cjustfile", "*.just", ".justfile" },
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
    {
        'L3MON4D3/LuaSnip',
        version = "v2.*",
        run = "make install_jsregexp"
    },
    'saadparwaiz1/cmp_luasnip',
    'rafamadriz/friendly-snippets',
    'onsails/lspkind.nvim',
    {
        'AckslD/swenv.nvim',
        dependencies = 'stevearc/dressing.nvim',
        config = function()
            require('swenv').setup({
                post_set_venv = function()
                    vim.cmd.LspRestart()
                end
            })
        end,
    },
    'simrat39/rust-tools.nvim',
    {
        'simrat39/symbols-outline.nvim',
        config = function()
            require("symbols-outline").setup({
                position = 'right',
                keymaps = {
                    close = "q",
                    fold = "n",
                    fold_all = "N",
                    unfold = "e",
                    unfold_all = "E"
                }
            })
        end
    },
    {
        'nvimtools/none-ls.nvim',
        config = function()
            local null_ls = require("null-ls")
            null_ls.setup({
                sources = {
                    null_ls.builtins.formatting.black,
                    null_ls.builtins.formatting.clang_format,
                }
            })
        end
    },
    'mfussenegger/nvim-jdtls',
    {
        'nvim-neotest/neotest',
        dependencies = {
            "nvim-lua/plenary.nvim",
            "nvim-treesitter/nvim-treesitter",
            "nvim-neotest/neotest-python",
            "antoinemadec/FixCursorHold.nvim"
        },
        config = function()
            require("neotest").setup({
                adapters = {
                    require("neotest-python")
                }
            })
        end
    },
    {
        "utilyre/barbecue.nvim",
        name = "barbecue",
        version = "*",
        dependencies = {
            "SmiteshP/nvim-navic",
            "nvim-tree/nvim-web-devicons", -- optional dependency
        },
        opts = {
            -- configurations go here
        },
    },

    -- Theme / UI
    {
        "folke/noice.nvim",
        event = "VeryLazy",
        opts = {
            lsp = {
                -- override markdown rendering so that **cmp** and other plugins use **Treesitter**
                override = {
                    ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
                    ["vim.lsp.util.stylize_markdown"] = true,
                    ["cmp.entry.get_documentation"] = true,
                },
                hover = {
                    silent = true,
                },
            },
            -- you can enable a preset for easier configuration
            presets = {
                bottom_search = false, -- use a classic bottom cmdline for search
                command_palette = true, -- position the cmdline and popupmenu together
                long_message_to_split = true, -- long messages will be sent to a split
                inc_rename = false, -- enables an input dialog for inc-rename.nvim
                lsp_doc_border = false, -- add a border to hover docs and signature help
            },
        },
        dependencies = {
            "MunifTanjim/nui.nvim",
            "rcarriga/nvim-notify",
        }
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
    {
        'm4xshen/smartcolumn.nvim',
        opts = {
            colorcolumn = "120",
            disabled_filetypes = {
                "help", "text", "markdown", "org", "lazy", "mason",
                "dashboard"
            },
            custom_colorcolumn = {
                python = "120"
            }
        },
    },
    {
        "lukas-reineke/indent-blankline.nvim",
        main = "ibl",
        opts = {
            indent = {
                char = "┆",
            },
            scope = { enabled = false },
        },
    }
})

vim.g.mapleader = " "
