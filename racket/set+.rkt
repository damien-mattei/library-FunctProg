#lang reader SRFI-105

;;#lang reader "../../Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/SRFI-105.rkt"

;; set definitions

;; Damien MATTEI


(module set racket

(provide singleton-set? dvs dvsos singleton? include? set-difference product-elem-with-set product-elem-with-set-tail-rec product-set-with-set product-set-with-set-imperative set-of-multiple-empty-sets? set-of-empty-set? display-sos display-set browse-set some? union length-sos product-set-with-set-imperative-sorted product-set-with-set-sorted product-elem-with-set-tail-rec-sorted)


(require Scheme+)
;;(require "../../Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/Scheme+.rkt")
;;(require "../../Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/main.rkt")


(include "../debug.scm")
(include "display-racket-scheme.scm")

(include "../list.scm")

;; the code below is copy/paste from the Guile code


;; (singleton-set? '(a)) -> #t
(define-syntax singleton-set?
  (syntax-rules ()
    ((_ lst)
     (null? (cdr lst)))))



;; display set, variable name and set
(define-syntax dvs 
  (syntax-rules ()
    ((_ var) (begin
	       ;;(display (symbol->string (quote var)))
	       (display (quote var))
	       (display " = ")
	       (newline)
	       (display-set var)
	       (newline)))))


;; display variable and set of sets
(define-syntax dvsos 
  (syntax-rules ()
    ((_ var) (begin
	       ;;(display (symbol->string (quote var)))
	       (display (quote var))
	       (display " = ")
	       (newline)
	       (display-sos var)
	       (newline)))))


(define (singleton? expr)
  (and (list? expr) (symbol? (first expr)) (null? (rest expr))))

; predicate to test the inclusion of a subset in a set
;; (include? '(a) '(a b)) -> #t
;;1 ]=> (include? '(a) '(a))          
;;Value: #t
;; (include? '(a) '((a))) -> #f ;; <- WARNING
;; (include? '((a)) '((a) (a (not b)) (a b) (b d) (a e (not b)) (a (not b) c)))) -> #t
;;  (include? '((b d)) '((a) (a (not b)) (a b) (b d) (a e (not b)) (a (not b) c))) -> #t
(define (include? subset set) ;; (include? cL1 L2)
  (cond
   ((null? subset) #t) ;; empty subset always include in a set
   ((null? set) #f) ;; if subset is not empty and set is empty then result is false
   (else (and (member (first subset) set)
	      (include? (rest subset) set))))) ;; c include in L2 AND L1 include in L2


;; set difference aA - B
;;  (set-difference '(1 2 4 5 6) '(4 5 6 2 8)) -> '(1)
(define (set-difference aA B)
  (cond ((null? aA)
	 '())
	((member (first aA) B) ;; a C-? B      C- means "include"
	 (set-difference (rest aA) B)) ;; set-difference(A,B)
	(else
	 (cons (first aA) (set-difference (rest aA) B))))) ;; { a ,  set-difference(A,B) }




;; (associate-elem-with-set 'a '(b c d)) -> '((a b) (a c) (a d))
;; DEPRECATED (utilise append ce qui est lent)
(define (associate-elem-with-set elem set)
  (if (null? set)
      set
      (append (list (list elem (first set))) (associate-elem-with-set elem (rest set)))))

;;  (product-elem-with-set 'a '(b c d))
;; = ((a b) (a c) (a d))
(define (product-elem-with-set elem set)
  (if (null? set)
      set
      (cons (list elem (first set)) (product-elem-with-set elem (rest set)))))

;; (product-elem-with-set-accumulator 'a '(b c d) '())
;; = ((a d) (a c) (a b))
(define (product-elem-with-set-tail-rec elem set acc)
  (if (null? set)
      acc
      (product-elem-with-set-tail-rec elem (rest set) (cons (list elem (first set)) acc))))

;; the same version but keep result sorted if input was
(define (product-elem-with-set-tail-rec-sorted elem set acc)
  (reverse (product-elem-with-set-tail-rec elem set acc)))


;; set multiplication : find all the combinations of the possible association of two elements
;;
;; (associate-set-with-set '(a b c) '(d e f g)) -> '((a d) (a e) (a f) (a g) (b d) (b e) (b f) (b g) (c d) (c e) (c f) (c g))
;; this use append and is slow
;; (define (associate-set-with-set set1 set2)
;;   (nodebug
;;    (display-nl "associate-set-with-set"))
;;   (if (null? set1)
;;       set1
;;       (append (associate-elem-with-set (first set1) set2) (associate-set-with-set (rest set1) set2))))


(define (associate-set-with-set set1 set2)
  (nodebug
   (display-nl "associate-set-with-set"))
  (if (null? set1)
      set1
      (append (associate-elem-with-set (first set1) set2) (associate-set-with-set (rest set1) set2))))

;; this use append and is slow
;; (define (product-set-with-set set1 set2)
;;   (if (null? set1)
;;       set1
;;       (append (product-elem-with-set (first set1) set2) (product-set-with-set (rest set1) set2))))


;; (define (product-set-with-set set1 set2)
;;   (if (null? set1)
;;       set1
;;       (append (product-elem-with-set-tail-rec (first set1) set2 '()) (product-set-with-set (rest set1) set2))))


;; (product-set-with-set '(a b c) '(d e f g))
;; = ((a g) (a f) (a e) (a d) (b g) (b f) (b e) (b d) (c g) (c f) (c e) (c d))
;; this one is still not tail recursive :
;; we go deep in product-set-with-set but stacking product-elem-with-set-tail-rec !
;; in Racket the memory must be increased
;; but there is no more append slowing down the code
(define (product-set-with-set set1 set2)
  (if (null? set1)
      set1
      (product-elem-with-set-tail-rec (first set1) set2 (product-set-with-set (rest set1) set2))))

;; the same version but keep result sorted if input was
(define (product-set-with-set-sorted set1 set2)
  (if (null? set1)
      set1
      (product-elem-with-set-tail-rec-sorted (first set1) set2 (product-set-with-set-sorted (rest set1) set2))))

;; (define set1 '(a b c))
;; (define set2 '(d e f g))
;; (product-set-with-set-imperative set1 set2)
;; ((c g) (c f) (c e) (c d) (b g) (b f) (b e) (b d) (a g) (a f) (a e) (a d))
;; scheme@(guile-user)> set1
;; (a b c)
;; scheme@(guile-user)> set2
;; (d e f g)

(def (product-set-with-set-imperative set1 set2)

     (nodebug
      (display-nl "product-set-with-set-imperative : begin"))
      
     ;; https://www.quora.com/What-happens-if-you-take-a-cartesian-product-of-an-empty-set-and-a-non-empty-set
     (when (null? set1) (return set1)) ;; empty set result
     (when (null? set2) (return set2)) ;; empty set result

     {result <- '()} 
     
     (repeat

      {a <- (first set1)}
      {set1 <- (rest set1)}

      {set2-iterator <- set2}

      (repeat

       {b <- (first set2-iterator)}
       {set2-iterator <- (rest set2-iterator)}
       {result <- (cons (list a b) result)}

       until (null? set2-iterator))

      until (null? set1))

     (nodebug
      (display-nl "product-set-with-set-imperative : end"))

     result)

;; the same version but keep result sorted if input was
(define (product-set-with-set-imperative-sorted set1 set2)
  (reverse (product-set-with-set-imperative set1 set2)))
      
       

;; (set-of-multiple-empty-sets? '(() () ())) -> #t
;; (set-of-multiple-empty-sets? '(() () (x) ())) -> #f
;; (set-of-multiple-empty-sets? '(())) -> #t
;; (set-of-multiple-empty-sets? '()) -> #f

(define (set-of-multiple-empty-sets? s)
  (if (null? s)
      #f
      (andmap null? s)))


;; scheme@(guile-user)> (set-of-empty-set? '(()))
;; $2 = #t
;; scheme@(guile-user)> (set-of-empty-set? '())
;; $3 = #f
(define (set-of-empty-set? s)
  (equal? s '(()) )  )

;; the same as previous with macro
;; scheme@(guile-user)> (macro-set-of-empty-set? '())
;; $2 = #f
;; scheme@(guile-user)> (macro-set-of-empty-set? '(()))
;; $3 = #t
(define-syntax macro-set-of-empty-set? 
  (syntax-rules ()
    ((_ s) (equal? s '(()) ))))


;; > (display-sos '(((1 0 x x) (1 x 0 x) (1 x x 0)) ((x x 1 1) (x 1 x 1) (1 x x 1) (1 x 1 x) (1 1 x x))))

;;{

;; {
;; (1 0 x x)
;; (1 x 0 x)
;; (1 x x 0)
;; }

;; {
;; (x x 1 1)
;; (x 1 x 1)
;; (1 x x 1)
;; (1 x 1 x)
;; (1 1 x x)
;; }

;; }
(define (display-sos sos)
  (display-nl "{")
  (newline)
  (map (lambda (s)
	 (display-set s)
	 (newline))
       sos)
  (display-nl "}"))




;; (length-sos '(((1 0 x x) (1 x 0 x) (1 x x 0)) ((x x 1 1) (x 1 x 1) (1 x x 1) (1 x 1 x) (1 1 x x))))
;; 8

(define (length-sos sos)
  (apply + (map length sos)))



;; (display-set '((x x 1 1) (x 1 x 1) (1 x x 1) (1 x 1 x) (1 1 x x)))
;;{
;; (x x 1 1)
;; (x 1 x 1)
;; (1 x x 1)
;; (1 x 1 x)
;; (1 1 x x)
;;}
(define (display-set s)

  (if (null? s)
      (display-nl "∅") ;;"{}")
      (begin
	(display-nl "{")
	(map (lambda (e)
	       (display e)
	       (newline))
	     s)
	(display-nl "}"))))



;; (browse-set '(a b c d e)) -> '((a b) (b c) (c d) (d e))
;; (browse-set '(a b c)) -> '((a b) (b c))
;; (browse-set '(a b)) -> '((a b))
;; (browse-set '(a)) -> '()
(define (browse-set s)
  (cond ((null? s) s)
	((null? (rest s)) '())
	(else (append (list (list (first s) (first (rest s)))) (browse-set (rest s))))))


;; returns #t if (function x) returns #t for 
;; some x in the list
(define some?
  (lambda (fct list)
    (and (pair? list)
	 (or
	  (fct (car list))
	  (some? fct (cdr list))))))



(define (union E F)        ; ensemble x ensemble --> ensemble, les matheux notent E U F
  (cond ((null? E) F)
        ((member (first E) F) (union (rest E) F))
        (else (cons (first E) (union (rest E) F)))))


) ; end of module


	 
