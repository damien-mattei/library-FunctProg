;; arrays

;; TODO : make a version vector with resizable arrays using classes
;; cf: https://www.gnu.org/software/guile/manual/html_node/GOOPS.html

;; the value v should be put before in a let to avoid multiple evaluation after macro expand
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



;; scheme@(guile-user)> (define arr (make-array-2d 10 7 0))
;; scheme@(guile-user)> (array-n-dim-ref arr 4 3)
;; 0
;; scheme@(guile-user)> (array-n-dim-set! arr 7 4 3)
;; scheme@(guile-user)> (array-n-dim-ref arr 4 3)
;; 7
(define-syntax array-n-dim-ref
  (syntax-rules ()
    ((_ array x) (vector-ref array x))
    ((_ array x y ...) (vector-ref (array-n-dim-ref array y ...) x))))


(define-syntax array-n-dim-set!
  (syntax-rules ()
    ((_ array val x) (vector-set! array x val))
    ((_ array val x y ...) (vector-set! (array-n-dim-ref array y ...) x val))))

    

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

;; function ? to be used with map
;; TODO: make n dimension versions recursive with variable number of parameters
(define (funct-array-2d-set! array x y val) (vector-set! (vector-ref array y) x val))

(define (funct-array-2d-ref array x y) (vector-ref (vector-ref array y) x))

;; scheme@(guile-user)> (array-ref-set! dyna 7 3 4)
;; $4 = 7
;; scheme@(guile-user)> (array-ref dyna 3 4)
;; $5 = 7
(define-syntax array-ref-set!
  (syntax-rules ()
    ((_ array expr x y) (let ((v expr))
			  (array-set! array v x y)
			  v))))


