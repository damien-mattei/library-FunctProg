(define-syntax $
  (syntax-rules ()
    ((_ ev)  ev)
    ((_ ev ...) (begin ev ...))))
