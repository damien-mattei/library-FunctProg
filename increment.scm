;; increment variable
;; this is different than add1 in DrRacket
;; Warning 1+ is not SRFI-105 compatible in Racket ,use incf instead
(define-syntax incf
  (syntax-rules ()
    ((_ x)   (begin (set! x (+ x 1))
		    x))))

(define-syntax add1
  (syntax-rules ()
    ((_ x)   (+ x 1))))
