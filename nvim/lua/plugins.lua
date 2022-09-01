-- Only required if you have packer configured as `opt`
vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
    -- Packer can manage itself
    use 'wbthomason/packer.nvim'

    -- editor
    use 'monaqa/dial.nvim'
    use 'tpope/vim-sensible'
    use 'tpope/vim-surround'
    use {
        'numToStr/Comment.nvim',
        config = function()
            require('Comment').setup()
        end
    }
    use {
        'windwp/nvim-autopairs',
        config = function() require("nvim-autopairs").setup{} end
    }
    use {
        'nvim-neotest/neotest',
        requires = {
            "rouge8/neotest-rust"
        },
        config = function()
            require("neotest").setup({
                adapters = {
                    require("neotest-rust")
                },
                icons = {
                  child_indent = "│",
                  child_prefix = "├",
                  collapsed = "─",
                  expanded = "╮",
                  failed = "!",
                  final_child_indent = " ",
                  final_child_prefix = "╰",
                  non_collapsible = "─",
                  passed = "O",
                  running = ".",
                  skipped = "x",
                  unknown = "?",
                },
            })
        end,
    }


    -- keymap
    use {
        'folke/which-key.nvim',
        config = function()
            require('which-key').setup({})
        end
    }
    use {
        'sudormrfbin/cheatsheet.nvim',
        config = function ()
            require("cheatsheet").setup({})
        end
    }
    use {
        'phaazon/hop.nvim',
        branch = 'v2',
        config = function()
            require'hop'.setup { keys = 'ntesiroamglpufywqjb' }
        end
    }
    use 'anuvyklack/hydra.nvim'

    -- Buffer/Window Management
    use {
        'romgrk/barbar.nvim',
        config = function()
            require('bufferline').setup {
                auto_hide = false,
            }
        end,
    }
    use 'sindrets/winshift.nvim'
    use 'szw/vim-maximizer'
    use 'mrjones2014/smart-splits.nvim'

    -- Git Integration
    use 'kdheepak/lazygit.nvim'
    use {
        'lewis6991/gitsigns.nvim',
        config = function()
            require('gitsigns').setup{}
        end
    }
    use {
        'ahmedkhalf/project.nvim',
        config = function()
            require('project_nvim').setup ({})
        end
    }

    -- LSP / Completion / Treesitter
    use {
        'nvim-telescope/telescope.nvim', tag = '0.1.0',
        config = function()
            local telescope = require('telescope')
            vim.g.theme_switcher_loaded = true
            local options = {
              defaults = {
                vimgrep_arguments = {
                  "rg",
                  "--color=never",
                  "--no-heading",
                  "--with-filename",
                  "--line-number",
                  "--column",
                  "--smart-case",
                },
                prompt_prefix = "   ",
                selection_caret = "  ",
                entry_prefix = "  ",
                initial_mode = "insert",
                selection_strategy = "reset",
                sorting_strategy = "ascending",
                layout_strategy = "horizontal",
                layout_config = {
                  horizontal = {
                    prompt_position = "top",
                    preview_width = 0.55,
                    results_width = 0.8,
                  },
                  vertical = {
                    mirror = false,
                  },
                  width = 0.87,
                  height = 0.80,
                  preview_cutoff = 120,
                },
                file_sorter = require("telescope.sorters").get_fuzzy_file,
                file_ignore_patterns = { "node_modules" },
                generic_sorter = require("telescope.sorters").get_generic_fuzzy_sorter,
                path_display = { "truncate" },
                winblend = 0,
                border = {},
                borderchars = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
                color_devicons = true,
                set_env = { ["COLORTERM"] = "truecolor" }, -- default = nil,
                file_previewer = require("telescope.previewers").vim_buffer_cat.new,
                grep_previewer = require("telescope.previewers").vim_buffer_vimgrep.new,
                qflist_previewer = require("telescope.previewers").vim_buffer_qflist.new,
                -- Developer configurations: Not meant for general override
                buffer_previewer_maker = require("telescope.previewers").buffer_previewer_maker,
                mappings = {
                  i = { ["<esc>"] = require("telescope.actions").close },
                },
              },
            }
            -- check for any override
            -- options = require("core.utils").load_override(options, "nvim-telescope/telescope.nvim")
            telescope.setup(options)
            telescope.load_extension('projects')
        end
    }
    use {
        'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate'
    }
    use 'p00f/nvim-ts-rainbow'
    use 'neovim/nvim-lspconfig'
    use {
        'williamboman/mason.nvim',
        config = function()
            require('mason').setup()
        end
    }
    use {
        'williamboman/mason-lspconfig.nvim',
        config = function()
            require('mason-lspconfig').setup()
        end
    }
    use 'hrsh7th/nvim-cmp'
    use 'hrsh7th/cmp-nvim-lsp'
    use 'hrsh7th/cmp-buffer'
    use 'hrsh7th/cmp-path'
    use 'L3MON4D3/LuaSnip'
    use 'saadparwaiz1/cmp_luasnip'
    use 'onsails/lspkind.nvim'
    use 'simrat39/rust-tools.nvim'
    use 'jose-elias-alvarez/null-ls.nvim'

    -- Theme / UI
    use 'nvim-lua/popup.nvim'
    use 'nvim-lua/plenary.nvim'
    use {
        'karb94/neoscroll.nvim',
        config = function()
            require("neoscroll").setup()
        end
    }
    use {
        'kyazdani42/nvim-tree.lua',
        config = function()
            require("nvim-tree").setup()
        end
    }
    use {
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
    }
    use 'ellisonleao/gruvbox.nvim'
    use 'rafamadriz/neon'
    use 'marko-cerovac/material.nvim'
    use 'folke/lsp-colors.nvim'
    use {
        'nvim-lualine/lualine.nvim',
        config = function()
            require('lualine').setup{
                options = {
                    theme = 'gruvbox'
                }
            }
        end
    }
    use 'kyazdani42/nvim-web-devicons'
    use {
        'folke/trouble.nvim',
        config = function()
            require("trouble").setup {}
        end
    }
    use {
        'lukas-reineke/indent-blankline.nvim',
        config = function ()
            require("indent_blankline").setup ({
                space_char_blankline = " ",
                show_current_context = true,
                show_current_context_start = true,
            })
       end
    }
    use {
        'norcalli/nvim-colorizer.lua',
        config = function ()
            require'colorizer'.setup()
        end
    }
end)
