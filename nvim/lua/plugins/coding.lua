return {
    -- Git Integration
    {
        "NeogitOrg/neogit",
        dependencies = {
            "nvim-lua/plenary.nvim",         -- required
            "sindrets/diffview.nvim",        -- optional - Diff integration
            "nvim-telescope/telescope.nvim", -- optional
        },
        opts = {
            kind = "split_above"
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
        'nvim-treesitter/nvim-treesitter',
        build = ':TSUpdate'
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
    -- Completion
    {
        "danymat/neogen",
        dependencies = "nvim-treesitter/nvim-treesitter",
        config = true,
        -- Uncomment next line if you want to follow only stable versions
        -- version = "*"
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
    -- language specific plugins
    {
        "NoahTheDuke/vim-just",
        event = { "BufReadPre", "BufNewFile" },
        ft = { "\\cjustfile", "*.just", ".justfile" },
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
    'simrat39/rust-tools.nvim',
    'folke/neodev.nvim',
}
