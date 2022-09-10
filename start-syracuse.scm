#lang racket

(require (rename-in racket/base [for for-rack])) ;; backup original Racket 'for'


;; init and start the syracuse program

(include "syntactic-sugar.scm")
(include "block.scm")
(include "debug.scm")
(include "list.scm")
(include "for_next_step.scm")
(include "increment.scm")
(include "racket/escape-char-racket-scheme.scm")
(include "display.scm")
(include "operation.scm")
(include "symbolic.scm")
(include "simplify.scm")
(include "binary-arithmetic.scm")
(include "pair.scm")
(include "number.scm")
(include "stat.scm")
(include "cycle.scm")

(require racket/format)

(include "syracuse.scm")
