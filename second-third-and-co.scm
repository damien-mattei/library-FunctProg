(define-syntax second
  (syntax-rules ()
    ((_ p) (cadr p))))

(define-syntax third
  (syntax-rules ()
    ((_ p) (caddr p))))


;;(cadddr '(1 2 3 4 5)) -> 4

(define-syntax fourth
  (syntax-rules ()
    ((_ p) (cadddr p))))

