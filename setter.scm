;; array setter are in array.scm and guile/array.scm
(define-syntax <-
  (syntax-rules ()
     
    ((_ var expr)  ;; (if (defined? var)
     ;;     (begin
     ;; 	 (display "← : variable set!") (newline)
     (set! var expr))))
