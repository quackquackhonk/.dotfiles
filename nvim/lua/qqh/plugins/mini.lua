return {
    {
        'echasnovski/mini.nvim',
        version = false,
        config = function()
            require("mini.ai").setup()
            require("mini.bracketed").setup()
            require("mini.operators").setup()
            require("mini.surround").setup()
        end
    },
}
