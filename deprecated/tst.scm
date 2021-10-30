;; scheme@(guile-user)> (tst (7))
;; $1 = 7
;; scheme@(guile-user)> (tst (7 8))
;; $2 = 8
;; scheme@(guile-user)> (tst (7 8 9))
;; $3 = 9
scheme@(guile-user)> (tst ())
(define-syntax tst
  (syntax-rules () 
    ;; Found no definitions. Just exit
    ((_ (exprs ...))
     (begin exprs ...))))

