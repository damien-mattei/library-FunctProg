
;; This file is part of Scheme+

;; Copyright 2021 Damien MATTEI

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;; scheme@(guile-user)> (def (foo) (when #t (return "hello") "bye"))
;; scheme@(guile-user)> (foo)
;;  "hello"

;; (def x)

;; TODO: study def of a recursive function
(define-syntax def
  
  (lambda (stx)
    
      (syntax-case stx ()

	;; multiple definitions without values assigned
	;; (def (x y z))
	((_ (var1 ...)) #`(begin (define var1 '()) ...))
	
	;;  (def (foo) (when #t (return "hello") "bye"))
        ((_ (<name> <arg> ...) <body> <body>* ...)
         (let ((ret-id (datum->syntax stx 'return)))
           #`(define (<name> <arg> ...)
               (call/cc (lambda (#,ret-id) <body> <body>* ...)))))

	;; single definition without a value assigned
	;; (def x)
	((_ var) #`(define var '()))

	;; (def x 7)
	((_ var expr) #`(define var expr))

	((_ err ...) #`(syntax-error "Bad def form"))

	)))



;; definition and assignment
;; { x <+ 7 } is equivalent to : (<- x 7) or (define x 7)
(define-syntax <+
  (syntax-rules ()
    ((_ var expr) (define var expr))))


(define-syntax <§
  (syntax-rules ()
    ((_ var expr)
     (begin
       (define-once var '())
       (set! var expr)))))




;; scheme@(guile-user)> {yy <$ 49}
;; scheme@(guile-user)> {yy <$ 50}
;; scheme@(guile-user)> (if #t {yy <$ 51} 'never)
;; scheme@(guile-user)> yy
;; 51 ;; yy is global !!!

;; (define-syntax <$
;;   (syntax-rules ()
;;     ((_ var expr) (let* ((modul (current-module))
;; 			 (exist-var (module-variable modul (quote var))))
;; 		    (if exist-var
;; 			(module-set! modul (quote var) expr)
;; 			(module-define! modul (quote var) expr))))))


;; (define-syntax <$
;;   (syntax-rules ()
;;     ((_ var expr) (begin
;; 		    (define modul (current-module)) ;; definition not allowed in expression context
;; 		    (define exist-var  (module-variable modul (quote var)))
;; 		    (if exist-var
;; 			(module-set! modul (quote var) expr)
;; 			(module-define! modul (quote var) expr))))))


;; scheme@(guile-user)> (foo)
;; 2
;; 1
;; (define (foo) 
;;   (define x 1) ;; x <- 1
;;   (if #t
;;       (let ()
;; 	(define x 2) ;; x <+ 2
;; 	(display x)
;; 	(newline))
;;       'never)
;;   (display x)
;;   (newline))

;; ;; scheme@(guile-user)> (bar)
;; ;; 2
;; ;; 2
;; (define (bar)
;;   (define x 1) ;; x <- 1
;;   (if #t
;;       (let ()
;; 	(set! x 2) ;; x <- 2
;; 	(display x)
;; 	(newline))
;;       'never)
;;   (display x)
;;   (newline))

;; (define (foo2) 
;;   (define-once  x 1)
;;   (if #t
;;       (let ()
;; 	(define-once x 2)
;; 	;;(set! x 2)
;; 	(display "x=")
;; 	(display x)
;; 	(newline))
;;       'never)
;;   (display x)
;;   (newline))

;; (define (bar2)
;;   (define x 1) 
;;   (if #t
;;       (let ()
;; 	(define-once x 2)
;; 	(display x)
;; 	(newline))
;;       'never)
;;   (display x)
;;   (newline))

;; (define (foo3) 
;;   (define-once  x 1)
;;   (if #t
;;       (let ()
;; 	(define-once x 2)
;; 	;;(set! x 2)
;; 	(display "x=")
;; 	(display x)
;; 	(newline)
;; 	(define-once x 3)
;; 	;;(set! x 2)
;; 	(display "x=")
;; 	(display x)
;; 	(newline)

;; 	)
;;       'never)
;;   (display x)
;;   (newline))



