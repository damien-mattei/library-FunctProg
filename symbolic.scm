(define (zero-symb? x)
  (and (number? x) (zero? x)))

(define (unity-symb? x)
  (and (number? x) (=  1 x)))


(define (symbolic-1 expr)
  (if (number? expr)
      (- expr 1)
      expr))
