;; various string function, someones thinked from SQL !!!

(define (get-char str)
  (string-ref str 0))


;;(trim-leading "    tttt") -> tttt
(define (trim-leading str)
  (define i (string-skip str #\space))
  (substring str i))

;; (trim-trailing "####  ") -> ####
(define (trim-trailing str)
  (define i (string-skip-right str #\space))
  (substring str 0 (+ i 1)))


;;(begin (display "|") (display (trim-both "    #####     ")) (display "|") (newline)) -> |#####|
(define (trim-both str)
  (trim-trailing (trim-leading str)))
