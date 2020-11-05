#lang racket

; predicate to check if a number map to another by an exposant
; (map-by-exp? 7 1) -> 11
; (map-by-exp? 7 2) -> #f
(define (map-by-exp? x e)
  (let [(c (((3 . * . x) . + . 1) . / . (2 . expt . e)))]
    (if (and (integer? c) (odd? c))
	c
	#f )))

; compute tuple if it exists for a starting number and the exponent list
; (compute-tuple 7  '(1 1 2)) -> '(7 11 17 13)
(define (compute-tuple x exp-seq)
  (if (null? exp-seq)
      (list x)
      (let [(c (map-by-exp? x (first exp-seq)))]
	(if (not c) ; not mapping by exponent
	    (list x)
	    (cons x (compute-tuple c (rest exp-seq)))))))

; compute tuple if it exists for a starting number and the exponent list
; more readable version
;  (compute-tuple-readable 7  '(1 1 2)) -> '(7 11 17 13)
(define (compute-tuple-readable x exp-seq)
  (let [(c -1)]
    (if (or (null? exp-seq) (begin (set! c (map-by-exp? x (first exp-seq))) (not c)))
	(list x)
	(cons x (compute-tuple c (rest exp-seq))))))

; compute tuple if it exists for a starting number and the exponent list
;
; (compute-tuple-non-efficiently 7  '(1 1 2)) -> '(7 11 17 13)
(define (compute-tuple-non-efficiently x exp-seq)
  (if (or (null? exp-seq) (not (map-by-exp? x (first exp-seq))))
      (list x)
      (cons x (compute-tuple (map-by-exp? x (first exp-seq)) (rest exp-seq)))))

; compute the tuple set for an exponent sequence and up to i-max level
; (compute-tuple-set 7 '(1 1 2)) ->
; 1 1 (1)
; 2 11 (3 5)
; 3 101 (5)
; (compute-tuple-set 15 '(1 1 2))
; 1 1    (1)
; 2 11   (3 5)
; 3 101  (5)
; 4 111  (7 11 17 13)
; 5 1001 (9)
; 6 1011 (11 17)
; 7 1101 (13)
(define (compute-tuple-set i-max exp-seq)
  (let [(n 0) (res '())]
    (for ((i (in-range 1 i-max 2)))
	 (set! n (+ n 1))
	 (display n) (display " ")
	 (set! res (compute-tuple i exp-seq))
	 (printf "~b " i) (display res) (printf "\n"))))

;; compute the distance functions d(1,i) and d(i)(see Peter Schorer web site http://occampress.com/)
;; > (take '(1 2 3 4 5) 3)
;; '(1 2 3)
;; > take
;; #<procedure:take>
(define (d1i i exp-seq)
  (let [(expos (take exp-seq i))
	(res 2)]
    (for [(expo expos)]
	 (set! res (* res (expt 2 expo))))
    res))

(define (dii i)
  (* 2 (expt 3 (- i 1))))
