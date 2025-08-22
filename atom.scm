
;; warning pair? should be replaced by something with list?
(define atom?
    (lambda (x)
      (and (not (pair? x)) (not (null? x)))))

