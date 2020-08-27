;; Guile compatible


;; see .guile file for options

;; does not work
;; export the variable below into shell before launching Guile

;; not sure ! :
;; export GUILE_AUTO_COMPILE=fresh

;; (load "test-postfix.scm")




(include "first-and-rest.scm")
(include "list.scm")
(include "display.scm")

;; if it does not works always well see important note above ! (export GUILE_AUTO_COMPILE=fresh)
;;(include "postfix.scm")


;; (define (tst2 L)
;;   (display L)
;;   (newline))


