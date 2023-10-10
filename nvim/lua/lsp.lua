-- Configuration for LSP / Treesitter / Completion

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
local util = require("lspconfig.util")

-- Setup lspconfig.
local capabilities = require('cmp_nvim_lsp').default_capabilities()

-- LSP Configuration
local augroup = vim.api.nvim_create_augroup("LspFormatting", {})
local custom_on_attach = function(client, bufnr)
    local bufopts = { noremap = true, silent = true, buffer = bufnr }
    if client.supports_method("textDocument/formatting") then
        vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
        -- vim.api.nvim_create_autocmd("BufWritePre", {
        --     group = augroup,
        --     buffer = bufnr,
        --     callback = function()
        --         -- on 0.8, you should use vim.lsp.buf.format({ bufnr = bufnr }) instead
        --         vim.lsp.buf.formatting_sync()
        --     end,
        -- })
    end

    --- diagnostic configs
    vim.diagnostic.config({
        virtual_text = true,
        float = false,
    })
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
    vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
    vim.keymap.set('n', '<Leader>lD', vim.lsp.buf.declaration, bufopts)
    vim.keymap.set('n', '<Leader>ld', vim.lsp.buf.definition, bufopts)
    vim.keymap.set('n', '<Leader>lt', vim.lsp.buf.type_definition, bufopts)
    vim.keymap.set('n', '<Leader>li', vim.lsp.buf.implementation, bufopts)
    vim.keymap.set('n', '<Leader>lc', vim.lsp.buf.rename, bufopts)
    vim.keymap.set('n', '<Leader>lr', '<cmd>Telescope lsp_references<cr>', bufopts)
    vim.keymap.set('n', '<Leader>l<Leader>', vim.lsp.buf.code_action, bufopts)
end

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics, {
    virtual_text = true,
    signs = true,
    underline = true,
    update_in_insert = false,
}
)

-- Formatting
local null_ls = require("null-ls")
null_ls.setup({
    -- you can reuse a shared lspconfig on_attach callback here
    on_attach = custom_on_attach,
    sources = {
        null_ls.builtins.formatting.black
    }
})

-- Rust
require('rust-tools').setup {
    tools = {
        autoSetHints = true,
        hover_with_actions = false,
        inlay_hints = {
            show_parameter_hints = true,
            parameter_hints_prefix = " <- ",
            other_hints_prefix = " => ",
        }
    },
    server = {
        on_attach = function(client, bufnr)
            custom_on_attach(client, bufnr)
            require('which-key').register({
                l = {
                    C = { ":RustOpenCargo<CR>", "Open Cargo.toml" }
                }
            }, { prefix = "<leader>" })
        end,
        capabilities = capabilities,
        settings = {
            ["rust-analyzer"] = {
                cargo = {
                    buildScripts = {
                        enable = true,
                    },
                },
                checkOnSave = {
                    command = "clippy",
                },
                imports = {
                    granularity = {
                        group = "module",
                    },
                    prefix = "self",
                },
                lens = {
                    enable = true,
                },
                procMacro = {
                    enable = true,
                },
            },
        },
    }
}


-- Lua
require 'lspconfig'.lua_ls.setup {
    settings = {
        Lua = {
            runtime = {
                -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
                version = 'LuaJIT',
            },
            diagnostics = {
                -- Get the language server to recognize the `vim` global
                globals = { 'vim' },
            },
            workspace = {
                -- Make the server aware of Neovim runtime files
                library = vim.api.nvim_get_runtime_file("", true),
                checkThirdParty = false,
            },
            -- Do not send telemetry data containing a randomized but unique identifier
            telemetry = {
                enable = false,
            },
        },
    },
}
-- Python
require 'lspconfig'.pyright.setup {
    on_attach = custom_on_attach,
    capabilities = capabilities,
    settings = {
        python = {
            analysis = {
                autoSearchPaths = true,
                diagnosticMode = 'openFilesOnly',
                useLibraryCodeForTypes = true,
                typeCheckingMode = 'basic'
            },
        }
    }
}

-- C/C++
require 'lspconfig'.clangd.setup {
    on_attach = custom_on_attach,
    capabilities = capabilities,
    filetypes = {"c", "cpp"},
}

require'lspconfig'.bufls.setup{}

-- Java configuration is in ftplugin/java.lua

-- Treesitter configuration
require 'nvim-treesitter.configs'.setup {
    -- A list of parser names, or "all"
    ensure_installed = "all",
    -- Install parsers synchronously (only applied to `ensure_installed`)
    sync_install = false,
    -- Automatically install missing parsers when entering buffer
    auto_install = true,
    highlight = {
        -- `false` will disable the whole extension
        enable = true,
        -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
        -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
        -- Using this option may slow down your editor, and you may see some duplicate highlights.
        -- Instead of true it can also be a list of languages
        additional_vim_regex_highlighting = false,
    },
    rainbow = {
        enable = true,
        extended_mode = false,
        colors = {
            colors.bright_orange,
            colors.bright_purple,
            colors.bright_green,
            colors.bright_blue,
        }
    }
}

-- Completion Config
vim.opt.completeopt = { "menu", "menuone", "noselect", "noinsert" }

-- Setup nvim-cmp.
local cmp = require 'cmp'
local luasnip = require 'luasnip'
require('luasnip.loaders.from_vscode').lazy_load()

-- helper function
local has_words_before = function()
    local line, col = unpack(vim.api.nvim_win_get_cursor(0))
    return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

cmp.setup({
    snippet = {
        -- REQUIRED - you must specify a snippet engine
        expand = function(args)
            require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
        end,
    },
    mapping = cmp.mapping.preset.insert({
        ['<C-e>'] = cmp.mapping.abort(),
        ["<Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
                cmp.select_next_item()
            elseif luasnip.expand_or_jumpable() then
                luasnip.expand_or_jump()
            elseif has_words_before() then
                cmp.complete()
            else
                fallback()
            end
        end, { "i", "s" }),
        ["<S-Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
                cmp.select_prev_item()
            elseif luasnip.jumpable(-1) then
                luasnip.jump(-1)
            else
                fallback()
            end
        end, { "i", "s" }),
        -- Accept currently selected item. Set `select` to `false` to
        -- only confirm explicitly selected items.
        ['<CR>'] = cmp.mapping.confirm({ select = true }),
    }),
    sources = {
        { name = 'nvim_lsp' },
        { name = 'buffer' },
    },
    formatting = {
        fields = { "kind", "abbr", "menu" },
        format = function(entry, vim_item)
            local kind = require("lspkind").cmp_format({ mode = "symbol_text", maxwidth = 50 })(entry, vim_item)
            local strings = vim.split(kind.kind, "%s", { trimempty = true })
            if #strings > 2 then
                kind.kind = " " .. strings[1] .. " "
                kind.menu = "    (" .. strings[2] .. ")"
            end
            return kind
        end,
    },
    window = {
        completion = {
            winhighlight = "Normal:Pmenu,FloatBorder:Pmenu,Search:None",
            col_offset = -3,
            side_padding = 0,
        },
    },
})
