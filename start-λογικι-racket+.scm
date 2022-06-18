#lang reader "racket/SRFI-105.rkt"

;; this is the bootstrap file for λογικι+ (LOGIKI)

;; Damien Mattei

;; 17/3/2022


;; Note: you can start the code directly from racket/logiki+.rkt if you want. (because include can not be used directly in SRFI-105 REPL for Racket)


(require "../Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/Scheme+.rkt")
;;(require "../Scheme-PLUS-for-Racket/Scheme+.rkt")

(require "racket/logiki+.rkt")

; DrRacket does not like greek names in filenames
;(include "program-λογικι-2.8.scm")


;; test
;;(define λογικι #t)

;; (infix-symb-min-dnf '(or (and (not a) (not b) (not c) (not d)) (and (not a) (not b) (not c) d) (and (not a) (not b) c (not d)) (and (not a) b (not c) d)  (and (not a) b c (not d))  (and (not a) b c d)  (and a (not b) (not c) (not d)) (and a (not b) (not c) d)  (and a (not b) c (not d))   (and c (not d))))

;; '((!b ^ !c) v (c ^ !d) v (!a ^ b ^ d))

;; '((¬b ∧ ¬c) ∨ (c ∧ ¬d) ∨ (¬a ∧ b ∧ d))
