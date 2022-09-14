;;#lang racket
;;#lang r5rs
;;#lang racket
;; CASE adapted for string comparison
;;
;; (load "/home/mattei/Dropbox/git/LOGIKI/lib/case.scm")
;; works with Kawa but not  Bigloo not, not R5RS 
;; (define-syntax case-string
  
;;   (syntax-rules ( => )
    

;;     ;; (define dog "dog")
;;     ;; (case-string dog (("dog") "animal")   (else => "mineral or vegetable"))
;;     ((case-string var
;; 		  lst
;; 		  (else => res-else  ...))
     
;;      (if (member var (car (quote lst)))
;; 	   (cadr (quote lst))
;; 	   (begin
;; 	     res-else
;; 	     ...)))
  
;;     ;; (define cat "cat")
;;     ;; (case-string cat (("dog") "animal") (("cat") "my daughter say cats are not animals!")  (else => "mineral or vegetable"))
;;     ((case-string var
;;     		  lst
;;     		  lst2
;;     		  ...
;;     		  (else => res-else ...))
     
;;      (if (member var (car (quote lst)))
;; 	 (cadr (quote lst))
;; 	 (case-string var
;; 		      lst2
;; 		      ...
;; 		      (else => res-else ...))))
  
   

;;     ;;(case-string dog  (else => "silly case"))
;;     ((case-string var
;; 		  (else => res-else ...)) 
     
;;      (begin
;;        res-else
;;        ...))))


;; (define-syntax case-string
  
;;   (syntax-rules ( => )
    

;;     ;; (define dog "dog")
;;     ;; (case-string dog (("dog") "animal")   (else => "mineral or vegetable"))
;;     ((case-string var
;; 		  lst
;; 		  (else => res-else))
     
;;      (begin
;;        (display "one argument case")
;;        (newline)
;;        (display " lst = ")
;;        (display (quote lst))
;;        (newline)
      
;;        (if (member var (car (quote lst)))
;; 	   (cadr (quote lst))
;; 	   res-else)))

;;     ;; (define cat "cat")
;;     ;; (case-string cat (("dog") "animal") (("cat") "my daughter think that cats are not animals !")  (else => "mineral or vegetable"))
;;     ((case-string var
;;     		  lst
;;     		  lst2
;;     		  ...
;;     		  (else => res-else))
     
;;      (begin
       
;;        (display "two or more arguments case")
;;        (newline)
;;        (display " lst = ")
;;        (display (quote lst))
;;        (newline)
;;        (if (member var (car (quote lst)))
;; 	   (cadr (quote lst))
;; 	   (case-string var
;; 			lst2
;; 			...
;; 			(else => res-else)))))
  
   

;;     ;;(case-string dog  (else => "silly case"))
;;     ((case-string var
;; 		  (else => res-else)) 
     
;;      res-else)))

;;  (foo (1 2 3) (4 5 6))
;; '(((1 4) (2 5) (3 6)))
;; > (foo (1 2 3) (4 5 6) (7 8 9))
;; '(((1 4) (2 5) (3 6)) ((1 7) (2 8) (3 9)))
;; > (foo (1 2 3) (4 5 6) (7 8 9) (10 11 12))
;; '(((1 4) (2 5) (3 6)) ((1 7) (2 8) (3 9)) ((1 10) (2 11) (3 12)))
;; (define-syntax foo 
;;   (syntax-rules () 
;;      ((foo (a ...) (b ...) ...) '(((a b) ...) ...))))


;; ;;  (foo2 ((1 2 3) (4 5 6)))
;; ;; '(((1 4) (2 5) (3 6)))
;; ;; > (foo2 ((1 2 3) (4 5 6)) ((7 8 9) (10 11 12)))
;; ;; '(((1 4) (2 5) (3 6)) ((7 10) (8 11) (9 12)))
;; ;;
;; ;; (foo2 ((1 2 3) (4 5 6)) ((7 8 9) (10 11 12)) ((13 14 15) (16 17 18)))
;; ;; '(((1 4) (2 5) (3 6)) ((7 10) (8 11) (9 12)) ((13 16) (14 17) (15 18)))
;; (define-syntax foo2 
;;   (syntax-rules () 
;;     ((foo2 ((a ...) (b ...)) ...) '(((a b) ...) ...))))


;; ;; (foo3 ((1 2 3) (4 5 6)) ((7 8 9) (10 11 12)) ((13 14 15) (16 17 18)))
;; ;; '(((1 . 4) (2 . 5) (3 . 6)) ((7 . 10) (8 . 11) (9 . 12)) ((13 . 16) (14 . 17) (15 . 18)))
;; (define-syntax foo3 
;;   (syntax-rules () 
;;     ((foo3 ((a ...) (b ...)) ...) '(((a . b) ...) ...))))


;; ;;> (foo4 (1 2 3) (4 5 6)) ->  '((1 . 4) (2 . 5) (3 . 6))

;;  (define-syntax foo4 
;;   (syntax-rules () 
;;     ((foo4 (a ...) (b ...)) '((a . b) ...))))



;; #|kawa:11|# (bar (1 2 3) (4 5 6))
;; ((1 4) (2 5) (3 6))
;; (define-syntax bar 
;;   (syntax-rules () 
;;      ((bar (a ...) (b ...))
;;       '((a b) ...))))


;; #|kawa:48|# (load "/home/mattei/Dropbox/git/LOGIKI/lib/case.scm")
;; #|kawa:49|# (toto (1 2) (3 4) 5)
;; (1 2 (3 4) (5))
;; #|kawa:50|# (toto (1 2) (3 4) 5 6 7)
;; (1 2 (3 4) 5 6 (7))
;; #|kawa:51|# (toto (1 2) (3 4) (5 6) 7)
;; (1 2 (3 4) (5 6) (7))

;; (define-syntax toto 
;;   (syntax-rules ()
   
;;     ((toto lst lst2 ... tail)
   
;;      ;;(quasiquote ((unquote (apply car (list (quote lst)))) (unquote (apply cadr (list (quote lst)))) lst2 ... (unquote (list tail)))))))
;;      (list (car (quote lst)) (cadr (quote lst)) (quote lst2) ... (list (quote tail))))))


(define-syntax case-member

        (syntax-rules (else)

	  ((case-member (key ...)
                        clauses ...)
	   (let ((atom-key (key ...)))
	     (case-member atom-key clauses ...)))

	  ((case-member key
                        (else result1 result2 ...))
	   (begin result1 result2 ...))

	  ((case-member key
                        ((atoms ...) result1 result2 ...))
	   (if (member key '(atoms ...))
	       (begin result1 result2 ...)))
	  
	  ((case-member key
			((atoms ...) result1 result2 ...) clause clauses ...)
	   (if (member key '(atoms ...))
	       (begin result1 result2 ...)
	       (case-member key clause clauses ...)))))



;; (define-syntax case-member2

;;   (syntax-rules (else)
    
;;     ((case-member2 (key ...)
;; 		   clauses ...)
     
;;      '(let ((atom-key (key ...)))
;; 	(case-member2 atom-key clauses ...)))
    

;;     ((case-member2 key
;; 		   (else result1 result2 ...))

;;      '(begin result1 result2 ...))


;;     ((case-member2 key
;; 		   ((atoms ...) result1 result2 ...))

;;      '(if (member key '(atoms ...))
;; 	  (begin result1 result2 ...)))
    

;;     ((case-member2 key
;; 		   ((atoms ...) result1 result2 ...) clause clauses ...)
     
;;      '(if (member key '(atoms ...))
;; 	  (begin result1 result2 ...)
;; 	  (case-member2 key clause clauses ...)))))
	  
