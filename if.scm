(define-syntax if-t
  (syntax-rules ()
    ((_ tst ev)  (if tst ev '()))
    ((_ tst ev ...)  (if tst (begin ev ...) '()))))
