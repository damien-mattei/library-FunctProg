;; useful macros 

;; author: Damien Mattei


;; DEPRECATED, for backward compatibility

;; "if" "then" without "else"
;;  (if-t (= 1 1) 'good) -> 'good
;;  (if-t (= 1 1) 'good 'bad) -> 'bad
;;  (if-t (= 1 1) 'good 'bad 'bof) -> 'bof
;; > (if-t (= 1 1) (display-nl 'good) (display-nl 'bad) (display-nl 'bof))
;; good
;; bad
;; bof
;; > (if-t (= 1 0) (display-nl 'good) (display-nl 'bad) (display-nl 'bof)) ->  '()
(define-syntax if-t
  (syntax-rules ()
    ((_ tst ev)  (if tst ev '()))
    ((_ tst ev ...)  (if tst (begin ev ...) '()))))

;; then and else do as begin-ners ;-)
(define-syntax then
  (syntax-rules ()
    ((_ ev)  ev)
    ((_ ev ...) (begin ev ...))))

;; commented? conflict with cond ?
;; (define-syntax else
;;   (syntax-rules ()
;;     ((_ ev)  ev)
;;     ((_ ev ...) (begin ev ...))))



(define-syntax while
  (syntax-rules (while)
    ((_ pred b1 ...)
     (let loop () (when pred b1 ... (loop))))))



;; > (define i 0) 
;; > (do (display-nl "toto") (set! i (+ i 1)) while (< i 4)) 
;; toto
;; toto
;; toto
;; toto
;; Warning: this 'do' break the one of scheme !
(define-syntax do
  (syntax-rules (while) ;;(do)
    ((do b1 ...
       while pred)
     (let loop () b1 ... (when pred (loop))))))



;; (define-syntax when
;;   (syntax-rules ()
;;     ((_ pred b1 ...)
;; when calls when ?????
;;      (when pred (begin b1 ...)))))



;; then and else do as BEGINners ;-)
;; (define-syntax then-block
;;   (syntax-rules ()
;;     ((_ ev)  ev)
;;     ((_ ev ...) (begin ev ...))))

;; (define-syntax else-block
;;   (syntax-rules ()
;;     ((_ ev)  ev)
;;     ((_ ev ...) (begin ev ...))))





;; > (for/break-cnd (i 1 3 (> i 1)) (display i) (newline) )
;; 1
;; 2
;; > (for/break (i 1 3 (> i x)) (let ((x 4)) (display i) (newline) ))
;; . for/break: bad syntax in: (for/break (i 1 3 (> i x)) (let ((x 4)) (display i) (newline)))
;; > (for/break-cnd (i 1 3 (> i x)) (let ((x 4)) (display i) (newline) ))
;; 1
;; . . x: undefined;
;;  cannot reference an identifier before its definition
;; > (let ((x 4)) (for/break-cnd (i 1 3 (> i x))  (display i) (newline) ))
;; 1
;; 2
;; 3
;; '()
;; (define-syntax for/break-cnd
;;   (syntax-rules ()
    
;;     ((_ (i from to cnd) b1 ...)

;;      (call-with-current-continuation
;;       (lambda (break)
;; 	(let loop ((i from))
;;        	  (when (<= i to)
;; 		(begin b1 ...
;; 		       (if cnd
;; 			   (break)
;; 			   (loop (incf i)))))))))))

;; (define-syntax for/break
;;   (syntax-rules ()
    
;;     ((_ (i from to) b1 ...)

;;      (call-with-current-continuation
;;       (lambda (break)
;; 	(let loop ((i from))
;;        	  (when (<= i to)
;; 		(begin b1 ...
;; 		       (loop (incf i))))))))))

;; works with racket not r5rs
;; (define-syntax loop/break
;;      (lambda (stx)
;;        (syntax-case stx ()
;;          ((?kwd (?var ?from ?to) ?body0 ?body ...)
;;           (with-syntax
;;               ((BREAK (datum->syntax #'?kwd 'break)))
;;             #'(call/cc
;;                   (lambda (escape)
;;                     (let-syntax
;;                         ((BREAK (identifier-syntax (escape))))
;;                       (let loop ((?var ?from))
;;                         (when (< ?var ?to)
;;                           (begin ?body0 ?body ...) ;; when already have a begin inside??? why one more ?
;;                           (loop (+ 1 ?var)))))))))
;;          )))

;; Bienvenue dans DrRacket, version 6.1.1 [3m].
;; Langage: racket [personnalisé]; memory limit: 256 MB.
;; > (for/break break (i 1 3) (display i) (newline) (when (> i 1) (break)) )
;; 1
;; 2
;; > (for/break break (i 1 3) (display i) (newline) (when (> i 1) (display-nl "hum")) )
;; 1
;; 2
;; hum
;; 3
;; hum
;; '()
;; > (for/break break (i 1 3) (display i) (newline) (when (= i 1) (display-nl "hum")) )
;; 1
;; hum
;; 2
;; 3
;; '()
;; > (for/break break (i 1 3) (display i) (newline) (when (= i 1) (break)) )
;; 1
;; > (for/break break (i 1 3) (display i) (newline) (when (= i 2) (break)) )
;; 1
;; 2
;; > (for/break break1 (i 1 3) (display i) (newline) (when (= i 2) (break1)) )
;; 1
;; 2
;; > (for/break break1 (i 1 3) (display i) (newline) (when (= i 2) (break)) )
;; 1
;; 2
;; . . break: undefined;
;;  cannot reference an identifier before its definition
;; >
;; > (for/break breaky (i 1 4) (for/break breakable (j 1 2) (display-nl i)))
;; 1
;; 1
;; 2
;; 2
;; 3
;; 3
;; 4
;; 4
;; > (for/break breaky (i 1 4) (for/break breakable (j 1 2) (display-nl i) (when (= i 2) (breakable))))
;; 1
;; 1
;; 2
;; 3
;; 3
;; 4
;; 4
;; > (for/break breaky (i 1 4) (for/break breakable (j 1 2) (display-nl i) (when (= i 2) (breaky))))
;; 1
;; 1
;; 2
;; >
;; > (define x 0)
;; > (for/break breaky (i 1 3) (set! x i) )
;; > x
;; 3
;;
;;  (for/break breaky (i 1 3) (if (= i 3)  (breaky i) '()) )
;; ;; 3
;; (define-syntax for/break
;;   (syntax-rules ()
;;     ((_ <break-id> (i from to) b1 ...)
;;      (call/cc (lambda (<break-id>)
;; 		(let loop ((i from))
;;         	  (when (<= i to)
;; 			(begin b1 ...
;; 			       (loop (incf i))))))))))




;; comment below for DrRacket Scheme,uncomment for MIT Scheme
;;(define (rest lst)
;;  (cdr lst))

;; for Bigloo Scheme :
;;(define (first lst)
;;   (car lst))

;; warning: first works with lists not pairs !!!!:
;; first: contract violation
;;   expected: (and/c list? (not/c empty?))
;;   given: '((x 1 x 1) . #f)
;;(define first car)

;; (define (second lst)
;;   (first (rest lst)))


;; (define-syntax add1
;;   (syntax-rules ()
;;     ((_ x)   (+ x 1))))



;; (define (symbol<? s1 s2)
;;   (string<? (symbol->string s1) (symbol->string s2)))

;; (define-syntax 1+
;;   (syntax-rules ()
;;     ((_ x)   (+ x 1))))

;; not-list? already do the same
;; (define (atom? x)
;;   (not (list? x)))

;; scheme@(guile-user)> (<- x 7)
;; scheme@(guile-user)> x
;; $5 = 7
;; scheme@(guile-user)> {x <- 8} 
;; scheme@(guile-user)> x
;; $6 = 8

;; function in array.scm powerful
;; (define-syntax <-
;;   (syntax-rules ()
;;     ((_ var expr) (set! var expr))))
    ;;((_ (array x) expr) (vector-set! array x expr))))
