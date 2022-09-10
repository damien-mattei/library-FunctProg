;; (use-modules ((for2)
;; 	      #:select ((break . for-break) continue for/bc))
;; 	     (srfi srfi-1) ; contains a break procedure
;; 	     (ice-9 receive))

(use-modules ((for_next_step)
	      #:select ((break . for-break) continue for/bc))
	     (srfi srfi-1) ; contains a break procedure
	     (ice-9 receive))


(let ((i #f))
   (for/bc ((set! i 0) (< i 10) (set! i (1+ i)))
     (receive (before after)
       (break (lambda (x)
                (> x 5))
              (iota i))
       (when (pair? after)
         for-break)
       (display i))))

