;;  (extract-cycle '(27 80 40 20 10 5 14 7) 20)   ->   '(20 10 5 14 7)
(define (extract-cycle L x)
  (cond ((null? L) L)
	((equal? x (first L)) L)
	(else (extract-cycle (rest L) x))))

;; (rotate-cycle-to-smallest-element '(20 10 5 14 7)) -> '(5 14 7 20 10)
(define (rotate-cycle-to-smallest-element L)
  (let* ((min-elem (apply min L))
	 (L-before-min (before-element min-elem L))
	 (L-start-min (start-with-element min-elem L)))
    (append L-start-min L-before-min)))


;; (get-cycle 20 '(27 80 40 20 10 5 14 7)) -> '(5 14 7 20 10)
(define (get-cycle x L)
  (rotate-cycle-to-smallest-element (extract-cycle L x)))
