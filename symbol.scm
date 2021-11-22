(define (symbol<? s1 s2)
  (string<? (symbol->string s1) (symbol->string s2)))

