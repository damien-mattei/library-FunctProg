(define almost-fac
 (lambda (:f :n)
  (if (= :n 1) 
   1 
   (* :n (:f :f (- :n 1))))))


(define Y (lambda (:f :n) (:f :f :n)))

;; scheme@(guile-user)> (Y almost-fac 6)
;; $4 = 720


;; scheme@(guile-user)> (almost-fac almost-fac 6)
;; $5 = 720
