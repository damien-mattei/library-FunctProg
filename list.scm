;; insert and set 
(define-syntax insert-set!
  (syntax-rules ()
    ((_ expr var)
     (set! var (insert expr var)))))


(define-syntax insert 
  (syntax-rules ()
    ((_ el lst) (cons el lst))))


(define (only-one? expr)
  (null? (rest expr)))

(define (pair-list? expr)
  (and (list? expr) (only-one? (rest expr))))

(define (empty? lst)
  (null? lst))

(define empty '())

;; insert an element in a list (at the end) if the element is not already included in the list
(define (insertNoDups element lst)
  (cond
   ((empty? lst) (cons element lst))
   ((equal? element (first lst)) lst)
   (else (cons (first lst) (insertNoDups element (rest lst))))))

;; (remove-duplicates '(a bc d e f a x y z d e t g)) -> '(g t e d z y x a f bc)
(define (remove-duplicates lst)
  (cond
   ((empty? lst) empty)
   (else (insertNoDups (first lst) (remove-duplicates (rest lst))))))


(define (singleton-list? lst)
  (and (list? lst) (null? (rest lst))))


;;> (create-list '() 5) -> '(() () () () ())
;;> (create-list 'x 5) -> '(x x x x x)
(define (create-list elem lgt)
  (if (= 0 lgt)
      '()
      (cons elem (create-list elem (- lgt 1)))))

;; remove duplicates but keep the list sorted
;;  (remove-duplicates-sorted '(A A B C D D E F G)) -> '(A B C D E F G)
(define (remove-duplicates-sorted sorted-lst)
  (reverse (remove-duplicates sorted-lst)))

;; like 'uniq' UNIX command but on List 
(define (uniq L)
  (cond
   ((null? L) '())
   (else
    (cons (car L)
	  (uniq (remove-firsts-elements (car L)
			(cdr L)))))))

;; remove all the c in '(c c c c c c ... L)
(define (remove-firsts-elements c L)
  (cond
   ((null? L) '())
   ;; ((null? (cdr L)) (if (equal? c (car L))
   ;; 			'()
   ;; 			L))
   ((not (equal? c (car L))) L)
   (else
    (remove-firsts-elements c (cdr L)))))
    
   

;; remove last element of a list
;;
;; > (remove-last '(((1 0 0 0)) ((0 1 0 1) (1 0 1 0) (1 1 0 0)) ((0 1 1 1) (1 1 0 1) (1 1 1 0)) ((1 1 1 1))))
;; '(((1 0 0 0)) ((0 1 0 1) (1 0 1 0) (1 1 0 0)) ((0 1 1 1) (1 1 0 1) (1 1 1 0)))
;; > (remove-last '(a b c))
;; '(a b)
(define (remove-last lst)
  (reverse (rest (reverse lst))))

;; > (replace '(1 (1 2 3 4 (5 6 3) 3 4)) 3 7)
;; '(1 (1 2 7 4 (5 6 7) 7 4))
;; > (replace '() 3 7)
;; '()
;; > (replace '(1 (1 2 3 4) 3 4) 3 7)
;; '(1 (1 2 7 4) 7 4)
;; (define (replace L new old)
;;   (cond ;;((null? L) L)
;; 	((list? L)
;; 	 (map
;; 	  (lambda (lst) (replace lst new old))
;; 	  L))
;; 	(else
;; 	 (if (equal? L old)
;; 	     new
;; 	     L))))

;; > (replace '(1 (1 2 3 4) 3 4) 3 7)
;; '(1 (1 2 7 4) 7 4)
;; > (replace '() 7 3)
;; '()
;; > (replace '(1 (1 2 3 4) 3 4) 3 7)
;; '(1 (1 2 7 4) 7 4)
;; > (replace '(1 (1 2 3 4 (5 6 3) 3 4)) 3 7)
;; '(1 (1 2 7 4 (5 6 7) 7 4))
;;
;;  (replace 4 4 5) -> 5
(define (replace L old new)
 
	(if (list? L)
	    (map
	     (lambda (lst) (replace lst old new))
	     L)
	    (if (equal? L old)
		new
		L)))


(define (debut n L)
  (if (or (null? L) (= n 0))
      '()
      (cons (first L) (debut (- n 1) (rest L)))))

(define (debut-iter n L)
  (define (iter acc ncur Lcur)
     (if (or (null? Lcur) (= ncur 0))
         acc
         (iter (cons (first Lcur) acc) (- ncur 1) (rest Lcur))))
  (reverse (iter '() n L)))
    
