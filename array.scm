;; arrays



(define-syntax make-array-2d
  (syntax-rules ()
    ((_ sx sy) (let* ((array (make-vector sy)))
		 (for (i 0 (- sy 1))
		      (vector-set! array i (make-vector sx)))
		 array))
    ((_ sx sy v) (let* ((array (make-vector sy)))
		   (for (i 0 (- sy 1))
			(vector-set! array i (make-vector sx v)))
		   array))
    ((_ array sx sy) (begin
		       (set! (quote array) (make-vector sy))
		       (for (i 0 (- sy 1))
			    (vector-set! (quote array) i (make-vector sx)))))
    ((_ array sx sy v) (begin
		       (set! (quote array) (make-vector sy))
		       (for (i 0 (- sy 1))
			    (vector-set! (quote array) i (make-vector sx v)))))))


(define-syntax array-2d-ref
  (syntax-rules ()
    ((_ array x y) (vector-ref (vector-ref array y) x))))

(define-syntax array-2d-set!
  (syntax-rules ()
    ((_ array x y val) (vector-set! (vector-ref array y) x val))))

(define-syntax display-array-2d
  (syntax-rules ()
    ((_ array)
     (for (y 0 (- (vector-length array) 1))
	  (display-nl (vector-ref array y))))))


;; > (define _quai 34)
;; > (dv _quai)
;; _quai = 34
(define-syntax dv-2d 
  (syntax-rules ()
    ((_ var) (begin
	       ;;(display (symbol->string (quote var)))
	       (display (quote var))
	       (display-nl " = ")
	       (display-array-2d var)
	       (newline)))))


(define (funct-array-2d-set! array x y val) (vector-set! (vector-ref array y) x val))

(define (funct-array-2d-ref array x y) (vector-ref (vector-ref array y) x))


