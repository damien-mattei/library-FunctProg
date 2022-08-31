

;; scheme@(guile-user)> (use-modules (Scheme+))
;; scheme@(guile-user)> (define i 0)
;; scheme@(guile-user)> (define do '())
;; scheme@(guile-user)> (while {i < 4}
;;                           do
;;                              (display i)
;;                              (newline)
;;                              {i <- {i + 1}})
;; 0
;; 1
;; 2
;; 3
;; $1 = #f

;; (while {i < 4}
;;    do
;;      (display i)
;;      (newline)
;;      {i <- {i + 1}})

(define-syntax while
  (syntax-rules (while do)
    ((_ pred do b1 ...)
     (let loop () (when pred b1 ... (loop))))))



