;; choose r5rs as language in Racket (incompatible with Racket ,see explanation below)
;; Warning: in language check also the option (show details) Initial bindings
;; depending the way you load/import files you must uncheck 'Disallow redefinition of initial bindings'
(load  "git/library-FunctProg/display.scm") ;; just here to define the macro print-nl:

;;(define-syntax display-nl 
;;  (syntax-rules ()
;;    ((_ msg)   (begin (display msg) (newline)))))

(load "trace-scheme-versus-python.scm")
