local options = {
	guicursor = "",
	mouse = "nvi",
	nu = true,
	termguicolors = true,
	timeoutlen = 250,
	updatetime = 300,
	-- editor
	syntax = "on",

	scrolloff = 10,
	cursorline = true,
	signcolumn = "yes",

	number = true,
	relativenumber = true,

	-- show the tabbar always
	showtabline = 2,

	-- dont hide anything from me
	conceallevel = 0,
	concealcursor = "nc",

	-- tab sizes
	tabstop = 4,
	softtabstop = 4,
	shiftwidth = 4,
	shiftround = true,

	-- never ever ever fold anything
	foldmethod = "manual",
	foldlevel = 99,
	expandtab = true,
	incsearch = true,

	-- change ordering of splits
	splitright = true,
	splitbelow = true,
	formatexpr = "v:lua.require'conform'.formatexpr()",
	-- Don't show the mode, since it's already in the status line
	showmode = false,

	-- Enable break indent
	breakindent = true,

	-- Save undo history
	undofile = true,

	-- Case-insensitive searching UNLESS \C or one or more capital letters in the search term
	ignorecase = true,
	smartcase = true,
	hlsearch = true,

	-- Preview substitutions live, as you type!
	inccommand = "split",

	-- sessions
	sessionoptions = "blank,buffers,globals,curdir,folds,help,tabpages,winsize,terminal",
}

for k, v in pairs(options) do
	vim.opt[k] = v
end

-- [[ Basic Autocommands ]]
--  See `:help lua-guide-autocommands`

-- Highlight when yanking (copying) text
--  Try it with `yap` in normal mode
--  See `:help vim.highlight.on_yank()`
vim.api.nvim_create_autocmd("TextYankPost", {
	desc = "Highlight when yanking (copying) text",
	group = vim.api.nvim_create_augroup("kickstart-highlight-yank", { clear = true }),
	callback = function()
		vim.highlight.on_yank()
	end,
})

vim.cmd("filetype plugin indent on")
vim.cmd([[autocmd BufNewFile,BufRead *.keymap setfiletype dts]])
vim.cmd([[autocmd FileType * set formatoptions-=o]])
vim.cmd("set t_ZH=[3m")
vim.cmd("set t_ZH=[23m")
vim.cmd("set fo-=l")
