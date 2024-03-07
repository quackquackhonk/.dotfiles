return {
    {
        'stevearc/oil.nvim',
        opts = {
            columns = {
                "icon",
                "permissions",
                "size",
                "mtime",
            },
            use_default_keymaps = true,
            keymaps = {
                ["g?"] = "actions.show_help",
                ["<CR>"] = "actions.select",
                ["<C-CR>"] = "actions.select_vsplit",
                ["<S-CR>"] = "actions.select_split",
                ["<C-t>"] = "actions.select_tab",
                ["<C-p>"] = "actions.preview",
                ["q"] = "actions.close",
                ["<C-l>"] = "actions.refresh",
                ["<BS>"] = "actions.parent",
                ["_"] = "actions.open_cwd",
                ["`"] = "actions.cd",
                ["~"] = "actions.tcd",
                ["gs"] = "actions.change_sort",
                ["gx"] = "actions.open_external",
                ["g."] = "actions.toggle_hidden",
                ["g\\"] = "actions.toggle_trash",
            },
        },
        -- Optional dependencies
        dependencies = { "nvim-tree/nvim-web-devicons" },
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
    {
        'nvim-treesitter/nvim-treesitter-context',
        opts = {
            max_lines = 3,
            multiline_threshold = 10,
            on_attach = function(_) return true end,
        }
    },
    {
        'declancm/maximize.nvim',
        opts = {
            default_keymaps = false
        }
    },
    -- Theme / UI
    {
        'folke/todo-comments.nvim',
        dependencies = { "nvim-lua/plenary.nvim" },
        opts = {
            keywords = {
                FIX = {
                    icon = " ", -- icon used for the sign, and in search results
                    color = "error", -- can be a hex color, or a named color (see below)
                    alt = { "FIXME", "BUG", "FIXIT", "ISSUE" }, -- a set of other keywords that all map to this FIX keywords
                },
                -- TODO: testing text
                TODO = { icon = " ", color = "#fabd2f" },
                -- HACK: testing text
                HACK = { icon = " ", color = "error" },
                -- WARN: testing text
                WARN = { icon = " ", color = "#fe8019", alt = { "WARNING", "XXX" } },
                -- PERF: testing text
                PERF = { icon = " ", color = "#d3869b", alt = { "OPTIM", "PERFORMANCE", "OPTIMIZE" } },
                -- NOTE: testing text
                NOTE = { icon = " ", color = "hint", alt = { "INFO" } },
            },
        }
    },
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
                bottom_search = true,         -- use a classic bottom cmdline for search
                command_palette = true,       -- position the cmdline and popupmenu together
                long_message_to_split = true, -- long messages will be sent to a split
                inc_rename = false,           -- enables an input dialog for inc-rename.nvim
                lsp_doc_border = false,       -- add a border to hover docs and signature help
            },
        },
        dependencies = {
            "MunifTanjim/nui.nvim",
        }
    },
    'nvim-lua/popup.nvim',
    'nvim-lua/plenary.nvim',
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
    'nvim-lualine/lualine.nvim',
    {
        'folke/trouble.nvim',
        config = function()
            require("trouble").setup {}
        end
    },
    'norcalli/nvim-colorizer.lua',
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
    "lukas-reineke/indent-blankline.nvim",
    'HiPhish/rainbow-delimiters.nvim',
    {

        'nanozuki/tabby.nvim',
        event = 'VimEnter',
        dependencies = 'nvim-tree/nvim-web-devicons',
    }

}
