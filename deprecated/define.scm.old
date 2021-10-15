;; Copyright 2020 Linus Björnstam
;;
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all source copies.
;; The software is provided "as is", without any express or implied warranties.
;;
;; Fix guile to have definitions in expression context in most common constructs.
;; nothing for srfi-11 so far. This is written using syntax-rules and should
;; be portable across implementation.



(define-module (guile define)
  #:use-module (ice-9 receive)
  #:replace ((new-lambda . lambda)
             (new-define . define)
             (new-begin . begin)
             (new-let . let)
             (new-let* . let*)
             (let-letrec . letrec)
             (new-letrec* . letrec*)
             (new-case . case)
             (new-cond . cond)
             ))



(define-syntax new-lambda
  (syntax-rules ()
    ((_ formals body body* ...)
     (lambda formals (new-begin body body* ...)))))

(define-syntax new-define
  (syntax-rules ()
    ((_ (name args ...) body body* ...)
     (define name (new-lambda (args ...) body body* ...)))
    ((_ name expr) (define name expr))))


(define-syntax new-begin
  (syntax-rules ()
    ((_ one-thing-only)
     one-thing-only)
    ((_ stuff ...)
     (%parse-body () stuff ...))))

(define-syntax new-let
  (syntax-rules ()
    ((_ clauses body body* ...)
     (let clauses (new-begin body body* ...)))))

(define-syntax new-let*
  (syntax-rules ()
    ((_ clauses body body* ...)
     (let* clauses (new-begin body body* ...)))))

(define-syntax new-letrec
  (syntax-rules ()
    ((_ clauses body body* ...)
     (letrec clauses (new-begin body body* ...)))))

(define-syntax new-letrec*
  (syntax-rules ()
    ((_ clauses body body* ...)
     (letrec* clauses (new-begin body body* ...)))))

(define-syntax new-case
  (syntax-rules (else =>)
    ;; Special case for else with a lambda-clause.
    ((_ expr
        (test body ...)
        (else => else-body ...))
     (case expr
       (test (new-begin body ...))
       (else => (new-begin else-body ...))))
    ((_ expr
        (test body ...) ...)
     (case expr
       (test (new-begin body ...)) ...))))




;; Exploits the fact that cond is just transformed into an if.
;; This means we can, as with case, just re-use guile's own
;; cond. Guile supports a non-standard guard-case. This should be removed
;; if porting to any other scheme.
(define-syntax new-cond
  (syntax-rules (=> else)
    ;; Guile-specific guard case
    ((_ (test guard => body ...) rest ...)
     (receive vals test
       (if (apply guard vals)
           (apply (new-begin body ...) vals)
           (new-cond rest ...))))

    ;; Lambda case
    ((_ (test => body ...) rest ...)
     (let ((temp test))
        (if test
            ((new-begin body ...) temp)
            (internal-cond rest ...))))

    ;; Else case
    ((_ (else body ...))
     (new-begin body ...))

    ;; No clauses left and no else clause.
    ((_) (if #f #f))

    ;; Normal case
    ((_ (test body ...) rest ...)
     (if test
          (new-begin body ...)
          (new-cond rest ...)))))



;;(%parse-define (clauses ...) body ...)
(define-syntax %parse-define
  (syntax-rules (new-define)
    ;; procedure definition
    ((_ (clauses ...) (new-define (name args ...) body body* ...) rest ... )
     (%parse-define (clauses ... (name (new-lambda (args ...) body body* ...))) rest ... ))

    ;; Variable definition
    ((_ (clauses ...) (new-define name expr) rest ...)
     (%parse-define (clauses ... (name expr)) rest ...))

    ;; Exit
    ((_ clauses rest ...)
     (letrec* clauses
       (%parse-body () rest ...)))))

;; A macro that transverses expressions
;; (%parse-body (seen-exprssions ...) rest ...)
(define-syntax %parse-body
  (syntax-rules (new-define)
    ;; Found no definitions. Just exit
    ((_ (exprs ...))
     (begin exprs ...))
    ;; A definition, exit to %parse-define
    ((_ (exprs ...) (new-define stuff ...) rest ...)
     (begin exprs ... (%parse-define () (new-define stuff ...) rest ... )))
    ;; Just a new expression.
    ((_ (exprs ...) new-expr rest ...)
     (%parse-body (exprs ... new-expr) rest ...))))

