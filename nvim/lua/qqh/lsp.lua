-- Configuration for LSP / Treesitter / Completion
-- Setup lspconfig.
local capabilities = require("cmp_nvim_lsp").default_capabilities()

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

    local wk = require("which-key")
    wk.register({
        K = { vim.lsp.buf.hover, "Hover", buffer = bufnr },
        ["<C-k>"] = { vim.lsp.buf.signature_help, "Signature Help", buffer = bufnr },
        g = {
            D = { vim.lsp.buf.declaration, "Goto declaration", buffer = bufnr },
            d = { vim.lsp.buf.definition, "Goto definition", buffer = bufnr },
            i = { vim.lsp.buf.implementation, "Goto implementations", buffer = bufnr },
            r = { vim.lsp.buf.rename, "Rename symbol", buffer = bufnr },
            R = { "<cmd>Telescope lsp_references<cr>", buffer = bufnr },
            ["<Leader>"] = { vim.lsp.buf.code_action, "Code actions", buffer = bufnr },
        },
    })
end

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
    virtual_text = true,
    signs = true,
    underline = true,
    update_in_insert = false,
})
-- Rust
vim.g.rustaceanvim = {
    -- Plugin configuration
    tools = {
        -- autoSetHints = true,
        -- hover_with_actions = false,
        inlay_hints = {
            show_parameter_hints = true,
            parameter_hints_prefix = " <- ",
            other_hints_prefix = " => ",
        },
    },
    -- LSP configuration
    server = {
        on_attach = function(client, bufnr)
            custom_on_attach(client, bufnr)
            require("which-key").register({
                l = {
                    C = { "<cmd>RustLsp openCargo<CR>", "Open Cargo.toml" },
                },
            }, { prefix = "<leader>" })
        end,
        settings = {
            -- rust-analyzer language server configuration
            ["rust-analyzer"] = {},
        },
    },
    -- DAP configuration
    dap = {},
}

-- Lua
require("lspconfig").lua_ls.setup({
    settings = {
        Lua = {
            runtime = {
                -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
                version = "LuaJIT",
            },
            diagnostics = {
                -- Get the language server to recognize the `vim` global
                globals = { "vim" },
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
})

-- Python
require("lspconfig").pyright.setup({
    on_attach = custom_on_attach,
    capabilities = capabilities,
    settings = {
        python = {
            analysis = {
                autoSearchPaths = true,
                diagnosticMode = "openFilesOnly",
                useLibraryCodeForTypes = true,
                typeCheckingMode = "basic",
            },
        },
    },
})
require("lspconfig").ruff_lsp.setup({
    init_options = {
        settings = {
            -- Any extra CLI arguments for `ruff` go here.
            args = {},
        },
    },
})
-- C/C++
require("lspconfig").clangd.setup({
    on_attach = custom_on_attach,
    capabilities = capabilities,
    filetypes = { "c", "cpp" },
    cmd = {
        "clangd",
        "--offset-encoding=utf-16",
    },
})

-- Nix
require("lspconfig").nil_ls.setup({})

-- protobuf
require("lspconfig").bufls.setup({})

-- Java configuration is in ftplugin/java.lua

-- Treesitter configuration
require("nvim-treesitter.configs").setup({
    textobjects = {
        select = {
            enable = true,
            lookahead = true,
            keymaps = {
                ["aF"] = "@function.outer",
                ["iF"] = "@function.inner",
            },
        },
    },
    -- A list of parser names, or "all"
    ensure_installed = { "rust", "lua", "python", "c", "cpp", "toml", "yaml", "json" },
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
    -- rainbow = {
    --     enable = true,
    --     extended_mode = false,
    --     colors = {
    --         colors.bright_orange,
    --         colors.bright_purple,
    --         colors.bright_green,
    --         colors.bright_blue,
    --     }
    -- }
})
