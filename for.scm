
(require (rename-in racket/base [for for-rack])) ;; backup original Racket 'for'

;; > (for (k 0 10) (display k) (newline))
;; 0
;; 1
;; 2
;; 3
;; 4
;; 5
;; 6
;; 7
;; 8
;; 9
;; 10
;; '()

;; > (for (k 0 10 3) (display k) (newline) (newline) (for (j 0 3) (display j) (newline)) (newline))
;; 0

;; 0
;; 1
;; 2
;; 3

;; 3

;; 0
;; 1
;; 2
;; 3

;; 6

;; 0
;; 1
;; 2
;; 3

;; 9

;; 0
;; 1
;; 2
;; 3

;; '()
;;

(define-syntax for
  (syntax-rules ()

    ;; ((_ ((i in-a-range)) b1 ...) ;; for DrRacket compatibility

    ;;  (for-rack ((i in-a-range)) b1 ...))
    
    ;; (let loop ((i 0))
    ;;   (when (< i to)
    ;; 	     b1 ...
    ;; 	     (loop (incf i)))))

    ((_ ((i to)) b1 ...) ;; for old compatibility
     
     (let loop ((i 0))
       (when (< i to)
	     b1 ...
	     (loop (incf i)))))
    
    ((_ (i to) b1 ...)
     
     (let loop ((i 0))
       (when (< i to)
	     b1 ...
	     (loop (incf i)))))

    
    
    ((_ (i from to) b1 ...)

     (let loop ((i from))
       (when (<= i to)    ;; warning test is inclusive ,different from case above !!!! must be fixed
       ;;(when (< i to)
	     b1 ...
	     (loop (incf i)))))
    
    ((_ (i from to step) b1 ...)
     
     (let loop ((i from))
       (when (<= i to)
       ;;(when (< i to)
	     b1 ...
	     (loop (+ i step)))))))


;; (for-next k = 0 to 10  (display k) (newline))
;; 0
;; 1
;; 2
;; 3
;; 4
;; 5
;; 6
;; 7
;; 8
;; 9
;; 10
;; > (for-next k = 0 to 5 step 2 (display k) (newline))
;; 0
;; 2
;; 4
;; > (for-next (k = 0 to 5 step 2) (display k) (newline))
;; 0
;; 2
;; 4
;; > (for-next (k = 0 to 5) (display k) (newline))
;; 0
;; 1
;; 2
;; 3
;; 4
;; 5
;; > 

;; (define-syntax for-next
;;   (syntax-rules (= to step)


;;     ;; The patterns are matched top down (the one with step must be before others patterns)
;;     ((for-next i = start to finish step inc b1 ...)
     
;;      (let loop ((i start))
;;        (when (<= i finish)
;; 	     b1 ...
;; 	     (loop (+ i inc)))))

    
;;     ((for-next i = start to finish b1 ...)

;;      (let loop ((i start))
;;        (when (<= i finish)
;; 	     b1 ...
;; 	     (loop (incf i)))))
    
   
    
;;     ((for-next (i = start to finish) b1 ...)

;;      (let loop ((i start))
;;        (when (<= i finish)
;; 	     b1 ...
;; 	     (loop (incf i)))))
    
;;     ((for-next (i = start to finish step inc) b1 ...)
     
;;      (let loop ((i start))
;;        (when (<= i finish)
;; 	     b1 ...
;; 	     (loop (+ i inc)))))))

;; deprecated
;; (define-syntax for-next-step
;;   (syntax-rules (= to step)
    
;;     ((for-next-step i = start to finish step inc b1 ...)
     
;;      (let loop ((i start))
;;        (when (<= i finish)
;; 	     b1 ...
;; 	     (loop (+ i inc)))))))
