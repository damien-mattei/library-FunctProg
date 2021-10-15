;; assignment version Guile (support my growable vectors)

;; This file is part of Scheme+

;; Copyright 2021 Damien MATTEI

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; scheme@(guile-user)> x
;; 7
;; scheme@(guile-user)> (let () (display x) (newline) (let () (<$ x 8) x))
;; 7
;; 8
;; scheme@(guile-user)> x
;; 7
;; scheme@(guile-user)> (let () (define x 3) (display "before :") (display x) (newline) (let () (<$ x 8) x) (display "after :") (display x) (newline) )
;; before :3
;; after :8
;; scheme@(guile-user)> x
;; 7
;; scheme@(guile-user)> (let () (<$ x 3) (display "before :") (display x) (newline) (let () (<$ x 8) x) (display "after :") (display x) (newline) )
;; before :3
;; after :8
;; scheme@(guile-user)> x
;; 7


;; scheme@(guile-user)> (foo)
;; <$ : global scope : x
;; before :3
;; <$ : displaced-lexical scope : x
;; before 2 :7
;; <$ : lexical scope : x
;; inside :8
;; after :8
;; 8

;; (define (foo)

;;   (let ()
;;     (<$ x 3)
;;     (display "before :")
;;     (display x)
;;     (newline)
;;     (<$ x 7)
;;     (display "before 2 :")
;;     (display x)
;;     (newline)
;;     (let ()
;;       (<$ x 8)
;;       (display "inside :")
;;       (display x)
;;       (newline)
;;       '())
;;     (display "after :")
;;     (display x)
;;     (newline)
;;     x))

;;(use-modules (system syntax))
;; cf : https://www.gnu.org/software/guile/docs/master/guile.html/Syntax-Transformer-Helpers.html
(define-syntax <$
  
  (lambda (s)
    
    (syntax-case s ()
      
      ((_ var value)
       
       (case (syntax-local-binding #'var)
	 
         ((lexical) #'(begin
			(display "<$ : lexical scope : ")
			(display (quote var))
			(newline)
			(set! var value)))
	 
	 ((displaced-lexical) #'(begin
				  (display "<$ : displaced-lexical scope : ")
				  (display (quote var))
				  (newline)
				  (set! var value)))
	 
         ((global) #'(begin
		       (display "<$ : global scope : ")
		       (display (quote var))
		       (newline)
		       ;;(define var value)))
		       {var <§ value}))
	 
         (else #'(begin
		   (display "<$ : unknow variable scope :")
		   (display (quote var))
		   (error "<$ : unknow variable scope : "))))))))




;; scheme@(guile-user)> {a[2 4] <- 7}
;; $1 = 7

;; scheme@(guile-user)> {a[2 4]}
;; $1 = 999
;; scheme@(guile-user)> {a[2 4] <- 7}
;; $2 = 7
;; scheme@(guile-user)> {a[2 4]}
;; $3 = 7
;; scheme@(guile-user)> {1 -> a[2 4]}
;; $4 = 1
;; scheme@(guile-user)> {a[2 4]}
;; $5 = 1
;; {x <- 2}
;;
;; (define T (make-vector 5))
;; scheme@(guile-user)> {T[3] <- 7}
;; <- : vector or array set!
;; $1 = 7
;;
;; scheme@(guile-user)> {T[3]}
;; $bracket-apply$
;; $3 = 7
;;
;; scheme@(guile-user)> {T[2] <- 4}
;; <- : vector or array set!
;; $2 = 4

;; scheme@(guile-user)> {T[3] <- T[2]}
;; $bracket-apply$
;; <- : vector or array set!
;; $4 = 4
;; scheme@(guile-user)> {T[3]}
;; $bracket-apply$
;; $5 = 4
(define-syntax <-
  (syntax-rules ()
    ;;  special form like : (<- ($bracket-apply$ T 3) ($bracket-apply$ T 4))
    
    ;; one dimension array, example: {a[4] <- 7}
    ;; in fact funct-or-macro is a MACRO and it is $bracket-apply$ of SRFI 105
    ;; TODO: scinder (_ ($bracket-apply$ array index) expr) et (_ (funct-or-macro array index) expr)
    ((_ (funct-or-macro array index) expr)
     (let ((tmp expr)) ;; to avoid compute it twice
						 
       ;; (if (equal? (quote $bracket-apply$) (quote funct-or-macro)) ;; test funct-or-macro equal $bracket-apply$
						 
       ;; normal case
       ;; {T[2] <- 4}
       ;; {T[3] <- T[2]}
       ;;(begin
	 ;;(display "<- : vector or array set!") (newline)
	 (if {(vector? array) or (growable-vector? array)}
	     (vector-set! array index tmp)
	     (array-set! array tmp index));)
       
       ;; rare case  (to prevent any error)
       ;; (let ((var (funct-or-macro array index))) ;; MUST be in a variable , otherwise:
       ;; While compiling expression:
       ;;  Syntax error:
       ;;  unknown location: quote: bad syntax in form quote
       ;; 	<- : variable set! after creation
       ;;  (set! var tmp)))
					     
	 tmp))


    ;; multi dimensions array :  {a[2 4] <- 7}
    ;; in fact funct-or-macro is a MACRO and it is $bracket-apply$ of SRFI 105
    ((_ (funct-or-macro array index ...) expr)
     (let ((tmp expr)) ;; to avoid compute it twice
  						 
       ;; (if (equal? (quote $bracket-apply$) (quote funct-or-macro)) ;; test funct-or-macro equal $bracket-apply$
       ;; normal case
       ;;(begin
	 ;;(display "<- : multidimensional vector or array set!") (newline)
	 (array-set! array tmp index ...);)
						     
	 ;; rare case (to prevent any error)
	 ;; (let ((var (funct-or-macro array index ...))) ;; MUST be in a variable
	 ;;   (display "<- : variable set! after creation (multidimensional)") (newline)
	 ;;   (set! var tmp)))
	 tmp))

    ;; compact form but will display a warning: possibly wrong number of arguments to `vector-set!'
    ;; and if i remove ellipsis it is a severe error
    ;; ((_ (funct-or-macro array index ...) expr) (let ((tmp expr)
    ;; 						     (var (funct-or-macro array index ...)))
    ;; 						 (if (equal? (quote $bracket-apply$) (quote funct-or-macro)) ;; test funct-or-macro equal $bracket-apply$
    ;; 						     ;; normal case
    ;; 						     (if {(vector? array) or (growable-vector? array)}
    ;; 							 (vector-set! array index ... tmp)
    ;; 							 (array-set! array tmp index ...))
						     
    ;; 						     ;; rare case (to prevent any error)
    ;; 						     (set! var tmp))
					     
    ;; 						 tmp))

    ;;(<- x 5)
    ((_ var expr)
     
    ;;  (begin
       
    ;;    (let* ((modul (current-module))
    ;; 	      (exist-var (module-variable modul (quote var))))
	 
    ;; 	 (if exist-var
	     
    ;; 	     (begin
    ;; 	       ;;(module-set! modul (quote var) expr)
    ;; 	       (display "assignment.scm : <- : existing variable") (newline)
    ;; 	       (set! var expr))
	
    ;; 	     ;;(begin
    ;; 	     (let ()
    ;; 	       ;;(module-define! modul (quote var) expr))))))
    ;; 	       (display "assignment.scm : <- : NON existing variable") (newline)
    ;; 	       (<$ var expr)
    ;; 	       (display "assignment.scm : <- : after macro <$") (newline)
    ;; 	       '())))

    ;;    (<$ var expr)
    ;;    var))))



     ;; (begin
     ;;   (define-once var '())
     ;;   (set! var expr)
     ;;   var))))

     (begin
       (<$ var expr)
       var))))


     ;; (let* ((modul (current-module))
     ;; 	    (exist-var (module-variable modul (quote var))))
     ;;   (if exist-var
     ;; 	   (module-set! modul (quote var) expr)
     ;; 	   ;;(set! var expr)) ;; do not work
     ;; 	   (module-define! modul (quote var) expr))
       
     ;;   var))))

         ;;(begin
	   ;;(display "<- : variable set!") (newline)
 	   ;;(set! var expr)
	   ;;(<$ var expr)
	   ;;var))))



(define-syntax </

  (lambda (s)
  
    (syntax-case s ()
      
      ;;  special form like : (<- ($bracket-apply$ T 3) ($bracket-apply$ T 4))
    
      ;; one dimension array, example: {a[4] <- 7}
       ;; in fact funct-or-macro is a MACRO and it is $bracket-apply$ of SRFI 105
       ;; TODO: scinder (_ ($bracket-apply$ array index) expr) et (_ (funct-or-macro array index) expr)
       ((_ (funct-or-macro array index) expr)
	#`(let ((tmp expr)) ;; to avoid compute it twice
						 
	  ;; (if (equal? (quote $bracket-apply$) (quote funct-or-macro)) ;; test funct-or-macro equal $bracket-apply$
	  
	  ;; normal case
	  ;; {T[2] <- 4}
	  ;; {T[3] <- T[2]}
	  ;;(begin
	  (display "<- : vector or array set!") (newline)
	  (if {(vector? array) or (growable-vector? array)}
	      (vector-set! array index tmp)
	      (array-set! array tmp index));)
       
	  ;; rare case  (to prevent any error)
	  ;; (let ((var (funct-or-macro array index))) ;; MUST be in a variable , otherwise:
	  ;; While compiling expression:
	  ;;  Syntax error:
	  ;;  unknown location: quote: bad syntax in form quote
	  ;; 	<- : variable set! after creation
	  ;;  (set! var tmp)))
					     
	  tmp))


    ;; multi dimensions array :  {a[2 4] <- 7}
    ;; in fact funct-or-macro is a MACRO and it is $bracket-apply$ of SRFI 105
    ((_ (funct-or-macro array index ...) expr)
     #`(let ((tmp expr)) ;; to avoid compute it twice
  						 
       ;; (if (equal? (quote $bracket-apply$) (quote funct-or-macro)) ;; test funct-or-macro equal $bracket-apply$
       ;; normal case
       ;;(begin
	 (display "<- : multidimensional vector or array set!") (newline)
	 (array-set! array tmp index ...);)
						     
	 ;; rare case (to prevent any error)
	 ;; (let ((var (funct-or-macro array index ...))) ;; MUST be in a variable
	 ;;   (display "<- : variable set! after creation (multidimensional)") (newline)
	 ;;   (set! var tmp)))
	 tmp))

    ;; compact form but will display a warning: possibly wrong number of arguments to `vector-set!'
    ;; and if i remove ellipsis it is a severe error
    ;; ((_ (funct-or-macro array index ...) expr) (let ((tmp expr)
    ;; 						     (var (funct-or-macro array index ...)))
    ;; 						 (if (equal? (quote $bracket-apply$) (quote funct-or-macro)) ;; test funct-or-macro equal $bracket-apply$
    ;; 						     ;; normal case
    ;; 						     (if {(vector? array) or (growable-vector? array)}
    ;; 							 (vector-set! array index ... tmp)
    ;; 							 (array-set! array tmp index ...))
						     
    ;; 						     ;; rare case (to prevent any error)
    ;; 						     (set! var tmp))
					     
    ;; 						 tmp))

    ;; (<- x 5)
    ((_ var expr)

     (case (syntax-local-binding #'var)
	 
         ((lexical) #'(let ((retval expr))
			(display "<- : lexical scope : ")
			(display (quote var))
			(newline)
			(set! var retval)
			retval))
	 
	 ((displaced-lexical) #'(let ((retval expr))
				  (display "<- : displaced-lexical scope : ")
				  (display (quote var))
				  (newline)
				  (set! var retval)
				  retval))
	 
         ((global) #'(begin
		       
		       (display "<- : global scope : ")
		       (display (quote var))
		       (newline)
		       
		       (let* ((retval expr)
			      (modul (current-module))
			      (exist-var (module-variable modul (quote var))))
	 
			 (if exist-var
			     
			     (begin
			       ;;(module-set! modul (quote var) expr)
			       (display "assignment.scm : <- : existing variable ") (display (quote var)) (newline)
			       (set! var retval))

			     (let ()
			       ;;(module-define! modul (quote var) expr))))))
			       (display "assignment.scm : <- : NON existing variable ") (display (quote var)) (newline)
			       (define var retval)
			       (display "assignment.scm : <- : after define") (newline)
			       '()))
			 (display "assignment.scm : <- : returning a value : ") (display retval) (newline)
			 retval)))
			     
	 
         (else #'(begin
		   (display "<- : unknow variable scope :")
		   (display (quote var))
		   (error "<- : unknow variable scope : "))))))))

    



(define-syntax ->
  (syntax-rules ()
    ;;  special form like : (-> ($bracket-apply$ T 3) ($bracket-apply$ T 4))
    ;; changé en (<- expr (funct-or-macro array index))
    ;;((_ expr (funct-or-macro array index)) {array[index] <- expr}  )
    ((_ expr (funct-or-macro array index)) (<- (funct-or-macro array index) expr))
    
    ;;((_ expr (funct-or-macro array index ...)) {array[index ...] <- expr} )
    ((_ expr (funct-or-macro array index ...)) (<- (funct-or-macro array index ...) expr))
    
    ;; (-> 5 x)
    ((_ expr var) {var <- expr})))








