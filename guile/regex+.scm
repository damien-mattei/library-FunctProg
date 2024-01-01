;; guile version

;; this is a limited version of Racket's regexp-match that only works for regular expression with cluster of size 2

;; Copyright (C) 2024  Damien MATTEI
;;
;;
;; e-mail: damien.mattei@gmail.com 

;; use :
;; (use-modules (regex+))


(define-module (regex+)
  #:use-module (Scheme+)
  #:use-module (ice-9 regex)
  #:export (regexp-match))


(include-from-path "debug.scm")
(include-from-path "escape-char-r7rs-scheme.scm")
(include-from-path "display-r6rs-scheme.scm")


;; (regexp-match "^([A-Za-z]+)([0-9]+)$" "c1")
;; ("c1" "c" "1")

;; (regexp-match "^([A-Za-z]+)([0-9]+)$" "toto")
;; #f

;; (regexp-match "^([A-Za-z]+)([0-9]+)$" "bb1n")
;; #f

;; (regexp-match "^([A-Za-z]+)([0-9]+)$" "CA10")
;; ("CA10" "CA" "10")

;; substring works also
;; scheme@(guile-user)> (regexp-match "^([A-Za-z]+)([0-9]+)$" "c₁₃")
;; ("c₁₃" "c" "₁₃")


;; scheme@(guile-user)> (regexp-match "^([A-Za-z]+)(-?[0-9]+)$" "c-1")
;; ("c-1" "c" "-1")

;; scheme@(guile-user)> (regexp-match "^([A-Za-z]+)([-₋]?[0-9]+)$" "c₋₁₃") ;; substring ₋₁₃
;; ("c₋₁₃" "c" "₋₁₃")
;; scheme@(guile-user)> (regexp-match "^([A-Za-z]+)([-₋]?[0-9]+)$" "c-₁₃")
;; ("c-₁₃" "c" "-₁₃")
;; scheme@(guile-user)> (regexp-match "^([A-Za-z]+)([-₋]?[0-9]+)$" "c-13")
;; ("c-13" "c" "-13")


(def (regexp-match re s)

     {vr <+ (string-match re s)}

     (nodebug
      (display-nl "regexp-match : ")
      (dv vr))

     (unless vr (return vr)) ;; assuming #f

     (when {(vector-length vr) ≠ 4} (return #f)) ;; unsupported inputs
     
     {start <+ (car vr[2])}
     {end <+ (cdr vr[2])}
     {var <+ (substring s start end)}
     
     {start <- (car vr[3])}
     {end <- (cdr vr[3])}
     {num <+ (substring s start end)}
     
     (list s var num))
  

