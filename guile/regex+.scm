;; guile version

;; this is a limited version of Racket's regexp-match that only works for regular expression with cluster of size 2

;; (regexp-match "^([A-Za-z]+)([0-9]+)$" "c1")
;; ("c1" "c" "1")

;; (regexp-match "^([A-Za-z]+)([0-9]+)$" "toto")
;; #f

;; (regexp-match "^([A-Za-z]+)([0-9]+)$" "bb1n")
;; #f

;; (regexp-match "^([A-Za-z]+)([0-9]+)$" "CA10")
;; ("CA10" "CA" "10")


(def (regexp-match re s)

     {vr <+ (string-match re s)}

     (nodebug
      (display-nl "regexp-match : ")
      (dv vr))

     (unless vr (return vr)) ;; assuming #f

     (when {(vector-length vr) <> 4} (return #f)) ;; unsupported inputs
     
     {start <+ (car {vr[2]})}
     {end <+ (cdr {vr[2]})}
     {var <+ (substring s start end)}
     
     {start <- (car {vr[3]})}
     {end <- (cdr {vr[3]})}
     {num <+ (substring s start end)}
     
     (list s var num))
  

