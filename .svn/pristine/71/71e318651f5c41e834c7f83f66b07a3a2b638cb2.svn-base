;; this file contains macro definitions by define-macro
;;
;; originally made for BiwaScheme and tested also with Bigloo Scheme
;;
;; author: Damien MATTEI
;; 
;; email: damien.mattei@unice.fr
;;


;; rewrote for Bigloo Scheme
;; (define-macro (print arg)
;;   `(begin
;;      (display ,arg)
;;      (newline)))


(define-macro (when test s1 . Larg)
  `(if ,test
       (begin ,s1 ,@Larg)
       #f))


;; (define i 0) 
;; (do-while (< i 4) (print "toto") (set! i (+ i 1)) ) 
;; toto
;; toto
;; toto
;; toto
(define-macro (do-while pred b1 . Larg)
  `(let loop ()
     (begin ,b1 ,@Larg)
     (when ,pred (loop))))


;; (do (print i) (if (< i 4) (print "inf") (print "sup")) (set! i (+ i 1)) while (< i 4))
;; 0
;; inf
;; 1
;; inf
;; 2
;; inf
;; 3
;; inf

(define-macro (do7 b1 . Larg)
   (let ((lst (gensym)) ;; list
	 (rev-lst (gensym)) ;; reversed list
	 (pred (gensym)) ;; predicate
	 (q-while (gensym)) ;; should be (quote while) or single while ?
	 (lst-instr (gensym))) ;; instructions to execute list

      `(let ((,lst (cons
		    (quote ,b1)
		    (quote ,Larg))) ;; list
	     (,rev-lst '()) ;; reversed list
	     (,pred '()) ;; predicate
	     (,q-while '()) ;;  should be (quote while) or single while ?
	     (,lst-instr '())) ;; instructions to execute list
	  
	  (if (< (length ,lst) 2)
	      "ERROR: too few arguments: must not be less than 2"
	      (begin
		 (set! ,rev-lst (reverse ,lst))
		 (set! ,pred (car ,rev-lst))
		 (set! ,q-while (car (cdr ,rev-lst)))
		 (if (not 
		      (or (equal? ,q-while (quote while))
			  (equal? ,q-while (quote WHILE))))
		     "ERROR: WHILE key-word not found"
		     (begin
			(set! ,lst-instr (reverse
					  (cdr 
					   (cdr ,rev-lst))))
			(let loop ()
			   (map eval ,lst-instr)
			   (if (eval ,pred) (loop))))))))))
			



(define-macro (do1 b1 . Larg)
   (let ((lst (gensym)) ;; list
	 (rev-lst (gensym)) ;; reversed list
	 (pred (gensym)) ;; predicate
	 (q-while (gensym)) ;; MUST be (quote while)
	 (lst-instr (gensym))) ;; instructions to execute list

      (print "do")
      `(let ((,lst (cons
		    (quote ,b1)
		    (quote ,Larg))) ;; list
	     (,rev-lst '()) ;; reversed list
	     (,pred '()) ;; predicate
	     (,q-while '()) ;; MUST be (quote while)
	     (,lst-instr '())) ;; instructions to execute list
	  
	  (print "passed")
	  (if (< (length ,lst) 2)
	      (print "ERROR: too few arguments: must not be less than 2")
	      (begin
		 (set! ,rev-lst (reverse ,lst))
		 (print ,rev-lst)
		 (set! ,pred (car ,rev-lst))
		 (set! ,q-while (car (cdr ,rev-lst)))
		 (print ,pred)
		 (print ,q-while)
		 (print "passed")
		 (if (not 
		      (or (equal? ,q-while (quote while))
			  (equal? ,q-while (quote WHILE))))
		     (print "ERROR: WHILE key-word not found")
		     (begin
			(print "passed 2")
			(set! ,lst-instr (reverse
					  (cdr 
					   (cdr ,rev-lst))))
			;;(set! ,lst-instr (list 'quote ,lst-instr)
			(print "passed set!")
			(print ,lst-instr)
			(let loop ()
			   (map eval ,lst-instr)
			   (if (eval ,pred) (loop))))))))))
			


(define-macro (do5 b1 . Larg)
   `(let ((lst (cons
		(quote ,b1)
		(quote ,Larg))) ;; list
	  (rev-lst '()) ;; reversed list
	  (pred '()) ;; predicate
	  (q-while '()) ;; MUST be (quote while)
	  (lst-instr '())) ;; instructions to execute list
       
       (print lst)
       (print "passed")
       (if (< (length lst) 2)
	   (print "ERROR: too few arguments: must not be less than 2")
	   (begin
	      (set! rev-lst (reverse lst))
	      (print rev-lst)
	      (set! pred (car rev-lst))
	      (set! q-while (car (cdr rev-lst)))
	      (print pred)
	      (print q-while)
	      (print "passed")
	      (if (not 
		   (or (equal? q-while (quote while))
		       (equal? q-while (quote WHILE))))
		  (print "ERROR: WHILE key-word not found")
		  (begin
		     (print "passed 2")
		     (set! lst-instr (reverse
				      (cdr 
				       (cdr rev-lst))))
		     (print "passed set!")
		     (print lst-instr)
		     (let loop ()
			(map eval lst-instr)
			(if (eval pred) (loop)))))))))

	      
(define-macro (do6 b1 . Larg)
   (let ((lst (cons b1 Larg)) ;; list
	 (rev-lst '()) ;; reversed list
	 (pred '()) ;; predicate
	 (q-while '()) ;; MUST be (quote while)
	 (lst-instr '())) ;; instructions to execute list
       
       (print lst)
       (print "passed")
       (if (< (length lst) 2)
	   (print "ERROR: too few arguments: must not be less than 2")
	   (begin
	      (set! rev-lst (reverse lst))
	      (print rev-lst)
	      (set! pred (car rev-lst))
	      (set! q-while (car (cdr rev-lst)))
	      (print pred)
	      (print q-while)
	      (print "passed")
	      (if (not 
		   (or (equal? q-while (quote while))
		       (equal? q-while (quote WHILE))))
		  (print "ERROR: WHILE key-word not found")
		  (begin
		     (print "passed 2")
		     (set! lst-instr (reverse
				      (cdr 
				       (cdr rev-lst))))
		     (print "passed set!")
		     (print lst-instr)
		     (let loop ()
			(map eval lst-instr)
			(if (eval pred) (loop)))))))))











	      




