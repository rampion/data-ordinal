" find files in the src and test directories
set path=.,src/**,test/**

" conveniently type greek characters
iabbr alpha α
iabbr beta β
iabbr gamma γ
iabbr omega ω
iabbr epsilon ε
iabbr delta δ
iabbr infinity ∞

let &makeprg="cabal"
nnoremap <Leader>m :make build<CR>
nnoremap <Leader>t :make test --show-details=always --test-option=--color<CR>

" skip ANSI escapes when detecting error locations
let &errorformat=" %f:%l:%c: %m,%f:%l:%c: %m"
