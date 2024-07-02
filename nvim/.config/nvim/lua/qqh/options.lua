local options = {
	guicursor = "",
	mouse = "nvi",
	nu = true,
	termguicolors = true,
	timeoutlen = 500,
	updatetime = 500,
	-- editor
	syntax = "on",
	cursorline = true,
	signcolumn = "yes",
	number = true,
	relativenumber = true,
	conceallevel = 2,
	concealcursor = "nc",
	textwidth = 99,
	foldexpr = "nvim_treesitter#foldexpr()",
	foldmethod = "expr",
	foldlevel = 99,
	sessionoptions = "blank,buffers,curdir,folds,help,tabpages,winsize,winpos,terminal",
	showtabline = 2,
	tabstop = 4,
	softtabstop = 4,
	shiftwidth = 4,
	shiftround = true,
	expandtab = true,
	incsearch = true,
	splitright = true,
	splitbelow = true,
	formatexpr = "v:lua.require'conform'.formatexpr()",
}

for k, v in pairs(options) do
	vim.opt[k] = v
end

vim.cmd("filetype plugin indent on")
vim.cmd([[autocmd BufNewFile,BufRead *.keymap setfiletype dts]])
vim.cmd([[autocmd FileType * set formatoptions-=o]])
vim.cmd("set t_ZH=[3m")
vim.cmd("set t_ZH=[23m")
vim.cmd("set fo-=l")
