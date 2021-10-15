;; Copyright 2020, 2021 Linus Björnstam
;;
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all source copies.
;; The software is provided "as is", without any express or implied warranties.

(define-syntax new-lambda
  (syntax-rules ()
    ((_ formals body body* ...)
     (lambda formals (parse-body (lambda formals body body* ...) body body* ...)))))

(define-syntax new-define
  (syntax-rules ()
    ((_ (stuff ...) body body* ...)
     (define (stuff ...) (parse-body (define (stuff ...) body body* ...) body body* ...)))
    ((_ name expr) (define name expr))))



(define-syntax new-let
  (syntax-rules ()
    ((_ (clauses ...) body body* ...)
     (let (clauses ...) (parse-body (let (clauses ...) body body* ...) body body* ...)))
    ((_ name (clauses ...) body body* ...)
     (let name (clauses ...)
       (parse-body (let name (clauses ...) body body* ...) body body* ...)))
    ((_ err ...) (syntax-error "Bad let form"))))

(define-syntax new-let*
  (syntax-rules ()
    ((_ (clauses ...) body body* ...)
     (let* (clauses ...) (parse-body (let (clauses ...) body body* ...) body body* ...)))
    ((_ err ...) (syntax-error "Bad let* form"))))

(define-syntax new-letrec
  (syntax-rules ()
    ((_ clauses body body* ...)
     (letrec clauses
       (parse-body (letrec clauses body body* ...) body body* ...)))
    ((_ err ...) (syntax-error "Bad letrec form"))))

(define-syntax new-letrec*
  (syntax-rules ()
    ((_ clauses body body* ...)
     (letrec* clauses
       (parse-body (letrec* clauses body body* ...) body body* ...)))
    ((_ err ...) (syntax-error "Bad letrec* form"))))

(define-syntax new-case
  (syntax-rules ()
    ((_ key clauses ...)
     (nca (case clauses ...) key () clauses ...))
    ((_ err ...) (syntax-error "Bad case form"))))

(define-syntax new-when
  (syntax-rules ()
    ((_ test body body* ...)
     (when test (parse-body (when test body body* ...) body body* ...)))
    ((err ...) (syntax-error "Bad when form"))))

(define-syntax new-unless
  (syntax-rules ()
    ((_ test body body* ...)
     (unless test (parse-body (unless test body body* ...) body body* ...)))
    ((err ...) (syntax-error "Bad unless form"))))



(define-syntax nca
  (syntax-rules (else =>)
    ((_ o k (clauses ...))
     (case k clauses ...))
    ((_ o k (c ...) (else => expr))
     (nca o k (c ... (else => expr))))
    ;; error checking the else clause:
    ((_ o k c (else => expr ...) more ...)
     (syntax-error "Malformed else clause" o))

    ((_ o k (c ...) (d => expr) . rest)
     (nca o k (c ... (d => expr)) . rest))
    ;; error checking the lambda clause
    ((_ o k c (d => expr ...) . rest)
     (syntax-error "Malformed case clause in form " (d => expr ...) o))
    ((_ o k (c ...) (d expr exprs ...) . rest)
     (nca o k (c ... (d (parse-body o expr exprs ...))) . rest))
    ((_ o  stuff ...) (syntax-error "Error in form " o))
    ((_ o err ...) (syntax-error "Bad case form" o))))



;; Exploits the fact that cond is just transformed into an if.
;; This means we can, as with case, just re-use guile's own
;; cond. Guile supports a non-standard guard-case. This should be removed
;; if porting to any other scheme.
(define-syntax new-cond
  (syntax-rules ()
    ((_ clauses ...)
     (nc (cond clauses ...) () clauses ...))))

(define-syntax nc
  (syntax-rules (else =>)
    ((nc o (clauses ...))
     (cond clauses ...))
    ((nc o (c ...) (else body ...))
     (nc o (c ... (else (parse-body o body ...)))))
    ((nc o (c ...) (test => expr) . rest)
     (nc o (c ... (test => expr)) . rest))
    ((nc o c (test => expr ...) . rest)
     (syntax-error "Bad cond clause form " (test => expr ...) o))
    ;; Remove these 2 clauses if porting to a scheme without this form
    ((nc o (c ...) (test guard => expr) . rest)
     (nc o (c ... (test guard => expr)) . rest))
    ((nc o c (test guard => expr ...) . rest)
     (syntax-error "Bad cond clause form " (test guard => expr ...) o))


    ((nc o (c ...) (test body body* ...) . rest)
     (nc o (c ... (test (parse-body o body body* ...))) . rest))

    ;; Error clauses
    ((nc o (c ...) (else body ...) error-clause error-clause* ...)
     (syntax-error "Else must be the last clause of a cond in form " o))
    ((_ o err ...) (syntax-error "Bad cond form" o))))



(define-syntax parse-body
  (syntax-rules (new-define)
    ((_ orig stuff ... (new-define stuff2 ...))
     (syntax-error "Body should end with an expression in form " orig))
    ((_ orig body ...)
     (pb2 orig () () body ...))))

(define-syntax pb2
  (syntax-rules (new-define)
    ((pb2 orig () (expr ...) expr*)
     (begin expr ... expr*))
    ((pb2 orig (clauses ...) (expr ...) expr*)
     (letrec* (clauses ...) expr ... expr*))


    ((pb2 orig (clauses ...) () (new-define (name args ...) body ...) . rest)
     (pb2 orig (clauses ... (name (new-lambda (args ...) body ...))) () . rest))
    ((pb2 o (clauses ...) () (new-define var body ...) . rest)
     (pb2 o (clauses ... (var (begin body ...))) () . rest))

    ((pb2 orig (clauses ...) (expr ...) (new-define (name args ...) body ...) . rest)
     (pb2 orig (clauses ... (_ (begin expr ...)) (name (new-lambda (args ...) body ...))) () . rest))
    ((pb2 o (clauses ...) (expr ...) (new-define var body ...) . rest)
     (pb2 o (clauses ... (_ (begin expr ...)) (var (begin body ...))) () . rest))

    ((pb2 o (clauses ...) (expr ...) expr* . rest)
     (pb2 o (clauses ...) (expr ... expr*) . rest))))
