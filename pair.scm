;; (add-pair '(5 . 4) '(5 . 4) '(8 . 3) '(8 . 4)) -> '(46 . 31)


;;  (add-pair (5 . 4) (5 . 4) (8 . 3) (8 . 4)) -> '(26 . 15)
(define-syntax add-pair
  (syntax-rules ()
    ;; ((_ p1 p2)
    ;;  (cons (+ (car p1) (car p2))
    ;; 	   (+ (cdr p1) (cdr p2))))
    
    ((_ (a . b) ...)
     (cons (+ a ...) (+ b ...)))))

    


;;(apply proc-add-pair '((5 . 4) (5 . 4) (8 . 3) (8 . 4)))  ->  '(26 . 15)
(define (proc-add-pair p . more-p)
  (let ((p-list (cons p more-p)))
    (cons (apply + (map car p-list))
	  (apply + (map cdr p-list)))))
