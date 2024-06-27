;; operation definitions

;; Damien MATTEI


;; ./curly-infix2prefix4guile.scm    --infix-optimize --infix-optimize-slice ../library-FunctProg/guile/operation+.scm > ../library-FunctProg/guile/operation.scm

;; install linux:
;; sudo cp operation.scm /usr/local/share/guile/site/3.0/operation+.scm

;; use :
;; (use-modules (operation+)) 


(define-module (operation+)
  #:use-module (Scheme+)
  #:use-module (set+)
  #:use-module (srfi srfi-1) ;; for 'first' procedure
  #:export (operator arg1 arg2 arg args function-without-parameters? unary-operation? binary-operation? prefix->infix prefix->infix-C-style prefix->infix-symb prefix->infix-symb-bool insert-op AND-op? OR-op? XOR-op? NOT-op? ADD-op? IMPLIC-op? EQUIV-op? isADD? MULTIPLY-op? isMULTIPLY? isOR? isAND? isOR-AND? isNOT? isIMPLIC? isEQUIV? isXOR? is-monomial-NOT? is-simple-form? DEFINE-op? ASSIGNMENT-op? is-associative-operator? isASSOCIATIVE? prefix-NOT->infix-symbolic-form prefix-NOT->infix-symbolic-form-greek prefix-NOT->infix-symbolic-form-bool bar-string string->bar-string alpha-op->symb-op alpha-op->symb-op-bool n-arity-operation->binary-operation is+? is*? is^? n-arity make-collect-leaves-operator collect-variables collect-var expt->^ is-True? is-False?)
  )

(define rest cdr)
(include-from-path "debug.scm")
(include-from-path "escape-char-r7rs-scheme.scm")
(include-from-path "display-r6rs-scheme.scm")
(include-from-path "list.scm")
(include-from-path "map.scm") ;; specialized for 'andmap'
(include-from-path "symbol.scm") ;; for symbol<?

;; return the operator of an operation
(define (operator expr)
  (car expr))

;; return the first argument of a binary operation
(define (arg1 expr)
  (first (rest expr)))

;; return the second argument of a binary operation
(define (arg2 expr)
  (first (rest (rest expr))))

(define (arg expr)
  (arg1 expr))

;; return the arguments of an operation
(define (args expr)
  (cdr expr))

;; (unary-operation? '(not a)) -> #t
(define (unary-operation? expr)
  (null? (rest (rest expr))))

;; (binary-operation? '(and a b)) -> #t
(define (binary-operation? expr)
  ;;(null? (rest (rest (rest expr)))))
  (and (pair? expr)
       (pair? (rest expr))
       (pair? (rest (rest expr)))
       (null? (rest (rest (rest expr))))))

;; prefix->infix
;; a self explanatory name
;; works only for now with DNF and CNF
;;
;; (prefix->infix '(and a b)) -> '(a and b)
;; (prefix->infix '(and a b c d e)) -> '(a and b and c and d and e)
;; (prefix->infix '(and a (not b) c d e)) -> '(a and !b and c and d and e)
;; (prefix->infix '(not (and a b)))  -> '|!(a and b)|
;; > (prefix->infix '(and a (not b) (not (or c d)) e))  -> '(a and !b and |!(c or d)| and e)
;; (prefix->infix (simplify-CNF (n-arity (cnf '(or (and c (not (or (and a (not b)) (and (not a) b)))) (and (not c) (or (and a (not b)) (and (not a) b))))))))
;; -> '((c or a or b) and (c or !b or !a) and (!a or b or !c) and (a or !b or !c))
;; (prefix->infix '(^ 2 4)) -> '(2 ^ 4)
;;
(define (prefix->infix expr)
  (cond
   ((null? expr) expr)
   ((number? expr) expr)
   ((boolean? expr) expr)
   ((symbol? expr) expr)
   ((isNOT? expr) (prefix-NOT->infix-symbolic-form expr)) ;; (prefix->infix '(not a )) -> '!a
   (else (insert-op (first expr) (rest (map prefix->infix expr))))))


;; (prefix->infix-C-style '(or (and (not a) (not b) (not c) (not d)) (and (not a) (not b) (not c) d) (and (not a) (not b) c (not d)) (and (not a) b (not c) d)  (and (not a) b c (not d))  (and (not a) b c d)  (and a (not b) (not c) (not d)) (and a (not b) (not c) d)  (and a (not b) c (not d))   (and c (not d))))
;; 
;; '((!a && !b && !c && !d)
;;   ||
;;   (!a && !b && !c && d)
;;   ||
;;   (!a && !b && c && !d)
;;   ||
;;   (!a && b && !c && d)
;;   ||
;;   (!a && b && c && !d)
;;   ||
;;   (!a && b && c && d)
;;   ||
;;   (a && !b && !c && !d)
;;   ||
;;   (a && !b && !c && d)
;;   ||
;;   (a && !b && c && !d)
;;   ||
;;   (c && !d))
(define (prefix->infix-C-style expr)
  (cond
   ((null? expr) expr)
   ((number? expr) expr)
   ((boolean? expr) expr)
   ((symbol? expr) expr)
   ((isNOT? expr) (prefix-NOT->infix-symbolic-form expr)) ;; (prefix->infix '(not a )) -> '!a
   (else
    (let ((op-in (first expr))
	  (op-out '()))
      (set! op-out
	    (cond
	     ((AND-op? op-in) '&&)
	     (else '||)))
      (insert-op op-out (rest (map prefix->infix-C-style expr)))))))



;; prefix to infix notation with symbolic operators (v  ^)
;; prefix vers infix  avec des expressions en forme symbolique (v ^)
;; (prefix->infix-symb (simplify-CNF (n-arity (cnf '(or (and c (not (or (and a (not b)) (and (not a) b)))) (and (not c) (or (and a (not b)) (and (not a) b))))))))
;; -> '((c v a v b) ^ (c v !b v !a) ^ (!a v b v !c) ^ (a v !b v !c))
;; (prefix->infix-symb (simplify-*NF (n-arity (dnf '(or (and c (not (or (and a (not b)) (and (not a) b)))) (and (not c) (or (and a (not b)) (and (not a) b))))))))
;; -> '((!a ^ !b ^ c) v (a ^ b ^ c) v (a ^ !b ^ !c) v (!a ^ b ^ !c))
;; (prefix->infix-symb (simplify (n-arity (cnf '(or (and c (not (or (and a (not b)) (and (not a) b)))) (and (not c) (or (and a (not b)) (and (not a) b))))))))
;; -> '((a v b v c) ^ (!a v !b v c) ^ (!a v b v !c) ^ (a v !b v !c))
;; 
;; (prefix->infix-symb (simplify (n-arity (phase3 (simplify-negation (move-in-negations (elim-implications '(or (and Cin (not (or (and A (not #t)) (and (not A) #t)))) (and (not Cin) (or (and A (not #t)) (and (not A) #t))))))))))) -> '((!A ^ Cin ^ F) v (A ^ Cin ^ T) v (A ^ !Cin ^ F) v (!A ^ !Cin ^ T) v (Cin ^ F ^ T))
(define (prefix->infix-symb expr)

  (nodebug
   (display "prefix->infix-symb : ")
   (dv expr))

  (declare result)
  
  (cond
   ((null? expr) {result <- expr})
   ((number? expr) {result <- expr})
   ((symbol? expr) {result <- expr})
   ((boolean? expr) {result <- (if expr 'T 'F)}) ;; True and False
   ((isNOT? expr) {result <- (prefix-NOT->infix-symbolic-form-greek expr)})
   (else {result <- (insert-op (alpha-op->symb-op (first expr))
			       (rest (map prefix->infix-symb expr)))}))

  (nodebug
   (dv result))

  result)



(define (prefix->infix-symb-bool expr)
  (cond
   ((null? expr) expr)
   ((number? expr) expr)
   ((symbol? expr) expr)
   ((boolean? expr) (if expr '■ '□)) ;; True and False
   ((isNOT? expr) (prefix-NOT->infix-symbolic-form-bool expr)) 
   (else (insert-op (alpha-op->symb-op-bool (first expr)) (rest (map prefix->infix-symb-bool expr))))))



;; insert operator between variables where it should be to make an infix expression
;; (insert-op '^ '(a b c d)) -> '(a ^ b ^ c ^ d)
;; TODO: see if we can use foldr (reduce en Lisp)
(define (insert-op op lst)
  ;; (insert-op 'and '(a b c d)) -> '(a and b and c and d)
  (if (null? (rest (rest lst)))
      (list (first lst) op (first (rest lst))) ;; (insert-op 'and '(a b)) -> '(a and b)
      (cons (first lst) (cons op (insert-op op (rest lst)))))) ;; (insert-op 'and '(a b c)) -> '(a and b and c)





;; test if an operator is AND
(define (AND-op? oper)
  ;;(or (equal? oper 'and) (equal? oper 'AND)))
  (or (eqv? oper 'and) (eqv? oper 'AND) (eqv? oper '·)))

;; test if an operator is OR
(define (OR-op? oper)
  ;;(or (equal? oper 'or) (equal? oper 'OR)))
  (or (eqv? oper 'or) (eqv? oper 'OR)  (eqv? oper '➕))) ;; middle dot

(define (XOR-op? oper)
  (or (eqv? oper 'xor) (eqv? oper 'XOR)  (eqv? oper '⊕))) ;; ⨁


;; test if an operator is NOT
(define (NOT-op? oper)
  (or (eqv? oper 'not) (eqv? oper 'NOT)))

(define (ADD-op? oper)
  (or (eqv? oper +) (eqv? oper '+)))

(define (IMPLIC-op? oper)
  (or (eqv? oper '⟹) (eqv? oper '=>)))

(define (EQUIV-op? oper)
  (or (eqv? oper '⟺) (eqv? oper '<=>)))



(define (isADD? expr)
  (and (pair? expr) (ADD-op? (car expr))))

(define (MULTIPLY-op? oper)
  (or (eqv? oper *) (eqv? oper '*)))

(define (isMULTIPLY? expr)
  (and (pair? expr) (MULTIPLY-op? (car expr))))

;; test if an expression is a OR
(define (isOR? expr)
  ;;(and (pair? expr) (equal? (car expr) 'or)))
  (and (pair? expr) (OR-op? (car expr))))

;; test if an expression is a AND
(define (isAND? expr)
  ;;(and (pair? expr) (equal? (car expr) 'and)))
  (and (pair? expr) (AND-op? (car expr))))


;; is expression an (OR or AND) ?
(define (isOR-AND? expr)
  (or (isOR? expr)  (isAND? expr)))

(define (isNOT? expr)
  (and (pair? expr) (NOT-op? (car expr))))

(define (isIMPLIC? expr)
  (and (pair? expr) (IMPLIC-op? (operator expr))))

(define (isEQUIV? expr)
  (and (pair? expr) (EQUIV-op? (operator expr))))

(define (isXOR? expr)
  (and (pair? expr) (XOR-op?  (operator expr))))


(define (is-True? expr)
  (or (equal? expr #t) (equal? expr 'T) (equal? expr '■) (equal? expr 'True)))


(define (is-False? expr)
  (or (equal? expr #f) (equal? expr 'F) (equal? expr '□) (equal? expr 'False)))


;; test for a monomial negation
;; (is-monomial-NOT? '(not x)) -> #t
;; (is-monomial-NOT? '(not (not x))) -> #f
(define (is-monomial-NOT? expr)
  (and (isNOT? expr) (symbol? (car (cdr expr)))))

;; predicate to know if an expression is a single form 
;;
;; examples of single forms: (not x) , (or x y z) , x
;; counter examples: (and (or x y))
;;
;; (is-simple-form? 'x) -> #t
;; (is-simple-form? '(not x)) -> #t
;; (is-simple-form? '(not (not x))) -> #f
;; (is-simple-form? '(and (or x y))) -> #f
;; (is-simple-form? '(or x y z)) -> #t
;; (is-simple-form?  '(and a b (not b))) -> #t
;;
(define (is-simple-form? expr)
  (cond
   ((boolean? expr) #t)
   ((symbol? expr) #t) ;; test for a single literal
   ((is-monomial-NOT? expr) #t) ;; test for a monomial negation
   ((isNOT? expr) #f)
   (else ;; we have a OR or AND
    ;; we check that all the arguments are literals or single negations
    (andmap
     (λ (q)
       (or (symbol? q) (is-monomial-NOT? q)))
     (args expr)))))


;; convert a prefix negation to a symbolic infix form
;; (prefix-NOT->infix-symbolic-form '(not a)) -> '!a
;; (prefix-NOT->infix-symbolic-form '(not (and a b))) -> '|!(a and b)|
(define (prefix-NOT->infix-symbolic-form expr)
  (let ((expr-arg (prefix->infix (first (rest expr))))) ;; get the literal symbol
    ;;(string->symbol (string-append "!" (format "~s" expr-arg))))) ;; construct '!expr-arg
    (string->symbol (string-append "!" (symbol->string expr-arg)))))

(define (prefix-NOT->infix-symbolic-form-greek expr)
  (let ((expr-arg (prefix->infix (first (rest expr))))) ;; get the literal symbol
    (string->symbol (string-append "¬" (symbol->string expr-arg))))) ;; Warning : this version can not handle expressions (but only symbols)


(define (prefix-NOT->infix-symbolic-form-bool expr)
  (let ((expr-arg (prefix->infix (first (rest expr))))) ;; get the literal symbol
    (string->symbol (string->bar-string (symbol->string expr-arg)))))

;; (bar-string #\Z)
;;  "Z̅"
(define (bar-string c)
  (string c (integer->char #x305))) ;; Macron

;; scheme@(guile-user)> (string->bar-string "AlPha")
;; $2 = "A̅l̅P̅h̅a̅"
;; scheme@(guile-user)> (string->bar-string "alpha")
;; $3 = "a̅l̅p̅h̅a̅"
;; scheme@(guile-user)> (string->bar-string "12345")
;; $4 = "1̅2̅3̅4̅5̅"
(define (string->bar-string s)
  {lg <+ (string-length s)}
  {sr <+ ""}
  (for-basic (i 0 {lg - 1})
       {c <+ {s[i]}} ;;(string-ref s i)}
       {scbar <+ (bar-string c)}
       {sr <- (string-append sr scbar)})
  sr)

;; convert from alphabetic operators to symbolic operators
(define (alpha-op->symb-op op)
  (cond
   ((AND-op? op) '∧) ;; on Emacs Ctrl-X 8 RET unicode
   ((OR-op? op) '∨)
   (else '?)))


;; convert from alphabetic operators to symbolic operators
(define (alpha-op->symb-op-bool op)
  (cond
   ((AND-op? op) '·) ;; on Emacs Ctrl-X 8 RET unicode
   ((OR-op? op) '➕)
   (else '?)))


;; (n-arity-operation->binary-operation '(and a b c)) -> '(and a (and b c))
;; (n-arity-operation->binary-operation '(and a (and b c) d)) -> '(and a (and (and b c) d))
;; (n-arity-operation->binary-operation '(and a b c d)) -> '(and a (and b (and c d)))
;; (n-arity-operation->binary-operation '(and a (and b c) (or d1 d3 d3 d4) (not e) #f))
;;   -> '(and a (and (and b c) (and (or d1 (or d3 (or d3 d4))) (and (not e) #f))))
;;
;; (n-arity (n-arity-operation->binary-operation '(and a (and b c) (or d1 d3 d3 d4) (not e) #f)))
;;   -> '(and a b c (or d1 d3 d3 d4) (not e) #f)
;;
;; (n-arity-operation->binary-operation '(and (=> (and p q) r) (=> (not (and p q)) r)))
;;   -> '(and (=> (and p q) r) (=> (not (and p q)) r))
;; (n-arity-operation->binary-operation '(+ a b c d)) -> '(+ a (+ b (+ c d)))
;; (n-arity-operation->binary-operation '(+ 1 2 3 4)) -> '(+ 1 (+ 2 (+ 3 4)))
;; (n-arity-operation->binary-operation '(xor #t #f #t #t)) -> '(xor #t (xor #f (xor #t #t)))
;;
(define (n-arity-operation->binary-operation expr)

  ;;(debug-mode-off)
  (when debug-mode
    (display "n-arity-operation->binary-operation : ")
    (dv expr))

  
  
  (cond
   ((boolean? expr) expr)
   ((number? expr) expr)
   ((symbol? expr) expr)
   ((unary-operation? expr) `(,(operator expr) ,(n-arity-operation->binary-operation (arg expr))))
   ((binary-operation? expr)  `(,(operator expr)
				,(n-arity-operation->binary-operation (arg1 expr))
				,(n-arity-operation->binary-operation (arg2 expr))))
   ;; else n-arity operation
   (else `(,(operator expr) ;; operator
	   ,(n-arity-operation->binary-operation (arg expr)) ;; 1 argument
	   ,(n-arity-operation->binary-operation `(,(operator expr) ,@(rest (rest expr)))))))) ;; n-1 arguments



(define (is+? expr)
  (eqv? '+ (first expr)))


(define (is*? expr)
  (eqv? '* (first expr)))


(define (is^? expr)
  (or (eqv? 'expt (first expr)) (eqv? '^ (first expr))))

;; n-arity function, this version will not show AND & OR case but collect them in one single line code
;; n-arity single function replacing n-arity-or and n-arity-and and that use the collect-leaves function 
;; with no match special form inside them and no operator show
;;
;;  (n-arity '(or a (or b c)))
;; '(or a b c)
;; > (n-arity '(or a (or b c d)))
;; '(or a b c d)
;; > (n-arity '(or a (and e f) (or b c d)))
;; '(or a (and e f) b c d)
;; > (n-arity '(or a (and e (or f g h i)) (or b c d)))
;; '(or a (and e (or f g h i)) b c d)
;; > (n-arity '(or a (and e (or f g h i)) (or b c d (or j k l))))
;; '(or a (and e (or f g h i)) b c d j k l)
;;
;; (n-arity '(or a (and e (or f g h i) (and m n)) (or b c d (or j k l))))
;; -> '(or a (and e (or f g h i) m n) b c d j k l)
;;
;; (n-arity '(not (or a (and e (or f g h i) (and m n)) (or b c d (or j k l)))))
;; '(not (or a (and e (or f g h i) m n) b c d j k l))
;; > (n-arity '(not (or a (and e (or f g (not h) i) (and m n)) (or b c d (or j k l)))))
;; '(not (or a (and e (or f g (not h) i) m n) b c d j k l))
;; > (n-arity '(not (or a (and e (or f g (not h) i) (and (not m) n)) (or (not b) c d (or j k l)))))
;; '(not (or a (and e (or f g (not h) i) (not m) n) (not b) c d j k l))

  
;;  > (n-arity (dnf '(or (and c (not (or (and a (not b)) (and (not a) b)))) (and (not c) (or (and a (not b)) (and (not a) b))))))
;;  '(or (and c (not a) a)
;;       (and c (not a) (not b))
;;       (and c b a)
;;       (and c b (not b))
;;       (and (not c) a (not b))
;;       (and (not c) (not a) b))

;;  > (n-arity '(or a (not b) (or (or (and c (and c2 c3)) d) e) (and (and (not f) g) h) (or i (and (not j) (and k (or l (or m (not n))))))) )
;;  '(or a (not b) (and c c2 c3) d e (and (not f) g h) i (and (not j) k (or l m (not n))))
;;  > 

  
;;  > (n-arity (cnf '(or (and c (not (or (and a (not b)) (and (not a) b)))) (and (not c) (or (and a (not b)) (and (not a) b))))))
;;  '(and (or c (not c))
;;        (or c a (not a))
;;        (or c a b)
;;        (or c (not b) (not a))
;;        (or c (not b) b)
;;        (or (not a) b (not c))
;;        (or (not a) b a (not a))
;;        (or (not a) b a b)
;;        (or (not a) b (not b) (not a))
;;        (or (not a) b (not b) b)
;;        (or a (not b) (not c))
;;        (or a (not b) a (not a))
;;        (or a (not b) a b)
;;        (or a (not b) (not b) (not a))
;;        (or a (not b) (not b) b))
;;     
;; (n-arity '(+ a (+ b c))) -> '(+ a b c)
;;
;; 
;;(prefix->infix (n-arity (expt->^ (simplify (hereditary-base-monomial-1 '(expt 4 7))))))
;; -> '((3 * (4 ^ 6)) + (3 * (4 ^ 5)) + (3 * (4 ^ 4)) + (3 * (4 ^ 3)) + (3 * (4 ^ 2)) + (3 * 4) + 3)
;;
(define (n-arity expr)


;;  (if ((isOR? expr) . or . (isAND? expr))
  ;; (if (or (isOR? expr) (isAND? expr))
  ;;     (let ((opera (operator expr)))
  ;;       (cons opera
  ;; 	      (apply
  ;; 	       append
  ;; 	       (map (make-collect-leaves-operator opera) (args expr)))))
  ;;     ;;(list expr)))
  ;;     expr))

  ;;(debug-mode-off)
  (when debug-mode
    (display "n-arity : ")
    (dv expr))

  
  
  (cond
   ((null? expr) expr)
   ((number? expr) expr)
   ((boolean? expr) expr)
   ((symbol? expr) expr)
   ((unary-operation? expr)
    (cons
     (operator expr)
     (list (n-arity (arg expr)))))
   ;;(else #;(binary-operation? expr) #;(or (isOR? expr) (isAND? expr))
    ((or (isOR? expr)
	 (isAND? expr)
	 (isADD? expr)
	 (isMULTIPLY? expr))
     (let ((opera (operator expr)))
       (cons opera
	     (apply
	      append
	      (map (make-collect-leaves-operator opera) (args expr))))))
   
    (else
     (let ((opera (operator expr)))
       (cons opera
	     (map n-arity (args expr)))))

	;;(list expr)))
    ;;#;(else expr)
    ))




;; return a closure of collect-leaves function associated with an operator (AND or OR)
(define (make-collect-leaves-operator oper)
  ;; TODO : faire une fonction genrique pour tous les operateurs
  ;;(if (AND-op? oper) 
      ;; TODO: try a definition with REC instead of LETREC
      ;; see: http://stackoverflow.com/questions/11231416/scheme-how-do-we-write-recursive-procedure-with-lambda
      ;; AND operator
      ;; (letrec ((collect-leaves-operator
      ;;           (lambda (expr)
      ;;             (cond
      ;; 		   ((isAND? expr) (apply append (map collect-leaves-operator (args expr))))
      ;; 		   ((isOR? expr) (list (n-arity expr)))
      ;; 		   (else (list expr))))))
      ;;   collect-leaves-operator)
      ;; 					;; OR operator
      ;; (letrec ((collect-leaves-operator
      ;;           (lambda (expr)
      ;;             (cond
      ;; 		   ((isOR? expr) (apply append (map collect-leaves-operator (args expr))))
      ;; 		   ((isAND? expr) (list (n-arity expr)))
      ;; 		   (else (list expr))))))
      ;;   collect-leaves-operator)))

  (let ((ourOperation?
	 (cond
	  ((AND-op? oper) isAND?)
	  ((OR-op? oper) isOR?)
	  ((ADD-op? oper) isADD?)
	  ((MULTIPLY-op? oper) isMULTIPLY?)
	  (else (error "unknow operator : " oper)))))

    
    (letrec ((collect-leaves-operator

	      (lambda (expr)
		(cond
		 ((null? expr) (list expr))
		 ((number? expr) (list expr))
		 ((symbol? expr) (list expr))
		 ((unary-operation? expr)
		  (list
		   (cons
		    (operator expr)
		    (list (n-arity (arg expr))))))
		 ((ourOperation? expr) ;; #;(eqv? oper (operator expr))
		  (apply append (map collect-leaves-operator (args expr))))
		 (else (list (n-arity expr)))))))

        collect-leaves-operator)))

;; return a closure of collect-leaves function associated with an operator (AND or OR)
;;(define (make-collect-leaves-operator-rec oper)
;;  ;; definition with REC instead of LETREC
;;  ;; see: http://stackoverflow.com/questions/11231416/scheme-how-do-we-write-recursive-procedure-with-lambda
;;  (if (AND-op? oper)  
;;      ;; AND operator
;;      (rec (collect-leaves-operator expr)
;;        (cond ((isAND? expr) (apply append (map collect-leaves-operator (args expr))))
;;             ((isOR? expr) (list (n-arity expr)))
;;             (else (list expr))))
;;      ;; OR operator
;;      (letrec ((collect-leaves-operator
;;                (lambda (expr)
;;                  (cond ((isOR? expr) (apply append (map collect-leaves-operator (args expr))))
;;                        ((isAND? expr) (list (n-arity expr)))
;;                        (else (list expr))))))
;;        collect-leaves-operator)))
;;      



;; (collect-variables '(or (and (and A B) (not (and #t (or (and A (not B)) (and (not A) B))))) (and (not (and A B)) (and #t (or (and A (not B)) (and (not A) B)))))) -> '(A B)
(define (collect-variables expr)
  (sort 
   (remove-duplicates 
    (cond
     ;;((boolean? expr) '())
     ((is-True? expr) '()) ;; they are not variables
     ((is-False? expr) '())
     ((symbol? expr) (list expr))
     ((number? expr) '())
     ((unary-operation? expr) (collect-variables (arg expr)))
     ((binary-operation? expr) (append (collect-variables (arg1 expr)) (collect-variables (arg2 expr))))
     (else (apply append (map collect-variables (args expr))))))
   symbol<?))

;; collect variables but no sorting nor remove duplicates
(define (collect-var expr)
  (cond
   ;;((boolean? expr) '())
   ((is-True? expr) '()) ;; they are not variables
   ((is-False? expr) '())
   ((symbol? expr) (list expr))
   ((number? expr) '())
   
   ((unary-operation? expr) (collect-var (arg expr)))
   ((binary-operation? expr) (append (collect-var (arg1 expr)) (collect-var (arg2 expr))))
   (else (apply append (map collect-var (args expr))))))

(define (expt->^ expr)
  (replace expr 'expt '^))


