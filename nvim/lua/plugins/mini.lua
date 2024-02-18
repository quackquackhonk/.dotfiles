return {
    {
        'echasnovski/mini.nvim',
        version = false,
        config = function()
            require("mini.ai").setup()
            require("mini.bracketed").setup()
            require("mini.operators").setup()
            require("mini.surround").setup()
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
}
