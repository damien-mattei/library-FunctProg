#lang racket

;; this is the bootstrap file for λογικι (LOGIKI)

;; Damien Mattei

;; 8/10/2018

;;(require racket/include)


(include "../library-FunctProg/syntactic-sugar.scm")

(include "../library-FunctProg/set.scm")

(include "../library-FunctProg/debug.scm")

(include "../library-FunctProg/racket/display-racket-scheme.scm")

(include "../library-FunctProg/array.scm") ;; TODO: use SRFI-25 instead 

(include "../library-FunctProg/symbolic.scm")

(include "../library-FunctProg/simplify.scm")

(include "../library-FunctProg/binary-arithmetic.scm")

(include "../library-FunctProg/for.scm")

(include "../library-FunctProg/map.scm")

(include "../library-FunctProg/list.scm")

(include "../library-FunctProg/operation.scm")

(include "../library-FunctProg/display-formula.scm")

(include "../library-FunctProg/minterms.scm")





(include "logiki-2.8.scm")

; DrRacket does not like greek names in filenames
;(include "program-λογικι-2.8.scm")


;; test
;;(define λογικι #t)

;; (infix-symb-min-dnf '(or (and (not a) (not b) (not c) (not d)) (and (not a) (not b) (not c) d) (and (not a) (not b) c (not d)) (and (not a) b (not c) d)  (and (not a) b c (not d))  (and (not a) b c d)  (and a (not b) (not c) (not d)) (and a (not b) (not c) d)  (and a (not b) c (not d))   (and c (not d))))

;; '((!b ^ !c) v (c ^ !d) v (!a ^ b ^ d))
