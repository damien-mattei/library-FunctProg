;;(require (rename-in racket/base [if if-backup]))
(require (only-in racket/base [if if-backup]))

(define-syntax if
  (syntax-rules ()
    ((_ tst ev)  (if-backup tst ev '()))
    ((_ tst ev-then ev-else)  (if-backup tst ev-then ev-else))))


