-- Only required if you have packer configured as `opt`
vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
    -- Packer can manage itself
    use 'wbthomason/packer.nvim'

    -- editing
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

    -- keymap
    use {
        'folke/which-key.nvim',
        config = function()
            require('which-key').setup({})
        end
    }

    -- Git Integration
    use 'kdheepak/lazygit.nvim'
    use {
        'ahmedkhalf/project.nvim',
        config = function()
            require('project_nvim').setup ({})
        end
    }

    -- LSP / Completion / Treesitter
    use {
        'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate'
    }
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

    -- Theme / UI
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
        'nvim-telescope/telescope.nvim', tag = '0.1.0',
        requires = { {'nvim-lua/plenary.nvim'} },
        config = function()
            local tele = require('telescope')
            tele.load_extension('projects')
        end
    }
end)
