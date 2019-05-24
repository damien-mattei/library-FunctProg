

(define (fac1 n)
  (if (= n 0)
      (begin
	(display-nl "fac1 : terminal case, returning 1")
	1)
      (begin
	(display "fac1 : computing (* ") (display n) (display " (fac1｜trace1 (- ") (display n) (display-nl " 1)))")
	(* n (fac1 (- n 1))))))


;; > (fac1 5)
;; fac1 : computing (* 5 (fac1｜trace1 (- 5 1)))
;; fac1 : computing (* 4 (fac1｜trace1 (- 4 1)))
;; fac1 : computing (* 3 (fac1｜trace1 (- 3 1)))
;; fac1 : computing (* 2 (fac1｜trace1 (- 2 1)))
;; fac1 : computing (* 1 (fac1｜trace1 (- 1 1)))
;; fac1 : terminal case, returning 1
;; 120
;; > (define fac1 (trace1 fac1))
;; > (fac1 5)
;;  trace1 -> fac1(5)
;; fac1 : computing (* 5 (fac1｜trace1 (- 5 1)))
;;   trace1 -> fac1(4)
;; fac1 : computing (* 4 (fac1｜trace1 (- 4 1)))
;;    trace1 -> fac1(3)
;; fac1 : computing (* 3 (fac1｜trace1 (- 3 1)))
;;     trace1 -> fac1(2)
;; fac1 : computing (* 2 (fac1｜trace1 (- 2 1)))
;;      trace1 -> fac1(1)
;; fac1 : computing (* 1 (fac1｜trace1 (- 1 1)))
;;       trace1 -> fac1(0)
;; fac1 : terminal case, returning 1
;;      trace1 : res = 1
;;     trace1 : res = 1
;;    trace1 : res = 2
;;   trace1 : res = 6
;;  trace1 : res = 24
;; trace1 : res = 120
;; 120
(define (trace1 f)
  (letrec ((n 0)
	   (tr_in (lambda (x)
		    (let ((res '()))
		      (begin
			(set! n (+ n 1))
			(display (make-string n #\ )) ;; display a gap of n spaces
			(display "trace1 -> fac1(") (display x) (display-nl ")")
			
			(set! res (f x)) ;;  call trace1 !
			
			(set! n (- n 1))
			(display (make-string n #\ )) (display "trace1 : res = ") (display-nl res)
			res)))))
		      
    tr_in))

(define (fac n)
  (if (= n 0)
      (begin
	(display-nl "fac : terminal case, returning 1")
	1)
      (begin
	(display "fac : computing (* ") (display n) (display " (fac｜trace (- ") (display n) (display-nl " 1)))")
	(* n (fac (- n 1))))))


;; Welcome to DrRacket, version 7.0 [3m].
;; Language: R5RS [custom]; memory limit: 128 MB.
;; > (fac 5)
;; fac : computing (* 5 (fac｜trace (- 5 1)))
;; fac : computing (* 4 (fac｜trace (- 4 1)))
;; fac : computing (* 3 (fac｜trace (- 3 1)))
;; fac : computing (* 2 (fac｜trace (- 2 1)))
;; fac : computing (* 1 (fac｜trace (- 1 1)))
;; fac : terminal case, returning 1
;; 120
;; > (define fac (trace fac))
;; > (fac 5)
;;  trace -> fac(5)
;; fac : computing (* 5 (fac｜trace (- 5 1)))
;;   trace -> fac(4)
;; fac : computing (* 4 (fac｜trace (- 4 1)))
;;    trace -> fac(3)
;; fac : computing (* 3 (fac｜trace (- 3 1)))
;;     trace -> fac(2)
;; fac : computing (* 2 (fac｜trace (- 2 1)))
;;      trace -> fac(1)
;; fac : computing (* 1 (fac｜trace (- 1 1)))
;;       trace -> fac(0)
;; fac : terminal case, returning 1
;;      trace : res = 1
;;     trace : res = 1
;;    trace : res = 2
;;   trace : res = 6
;;  trace : res = 24
;; trace : res = 120
;; 120
;; > 

(define (trace f)
  (letrec ((n 0)
	   (tr_in (lambda (arg1 . list-of-other-args)
		    (let ((args (cons arg1 list-of-other-args))
			  (res '()))
		      (begin
			(set! n (+ n 1))
			(display (make-string n #\ )) ;; display a gap of n spaces
			(display "trace -> fac") (display-nl args)
			
			(set! res (apply f args)) ;;  call trace !
			
			(set! n (- n 1))
			(display (make-string n #\ )) (display "trace : res = ") (display-nl res)
			res)))))
		      
    tr_in))

(define fac-tst (letrec ((fac-x (lambda (n)
				  (if (= n 0)
				      1
				      (* n (fac-x (- n 1)))))))
		  fac-x))


