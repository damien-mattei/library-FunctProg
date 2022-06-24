#lang racket

(require (rename-in racket/base [for for-rack])) ;; backup original Racket 'for'


;; init and start the syracuse program

(include "syntactic-sugar.scm")
(include "debug.scm")
(include "list.scm")
(include "operation.scm")
(include "symbolic.scm")
(include "simplify.scm")
(include "racket/escape-char-racket-scheme.scm")
(include "display.scm")
(include "increment.scm")
(include "binary-arithmetic.scm")
(include "for-next-step.scm")
(include "pair.scm")
(include "number.scm")
(include "stat.scm")
(include "cycle.scm")

(require racket/format)

(include "syracuse.scm")
