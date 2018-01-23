(define error-3-args error)

(define (error reason arg1 #;arg2 . more-args)
  (display "Error: ")
  (display reason)
  (display " ")
  (display arg1)
  ;;(display " ")
  ;;(display arg2)
   
  (for-each (lambda (arg) 
	      (display " ")
	      (write arg))
	    more-args)
  (newline)
  ;;(scheme-report-environment -1))  ;; we hope that this will signal an error
  
  ;; call the original bigloo function
  (error-3-args reason arg1 '()))
