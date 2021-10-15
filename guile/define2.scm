;; Copyright 2020, 2021 Linus Björnstam
;;
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all source copies.
;; The software is provided "as is", without any express or implied warranties.
;;
;; Fix guile to have definitions in expression context in most common constructs.
;; For guile 3 it should have no overhead, for guile 2 the code produced is slower
;; than the same code using let or let* due to guile 2 not optimizing letrec or
;; internal definitions.



(define-module (syntax define)
  #:replace ((new-lambda . lambda)
             (new-define . define)
             (new-let . let)
             (new-let* . let*)
             (new-letrec . letrec)
             (new-letrec* . letrec*)
             (new-when . when)
             (new-unless . unless)
             (new-case . case)
             (new-cond . cond)))
(include "define-impl.scm")
