;; pretty display sans les parentheses de ( expr )
;;=> (pretty-display  (dnf-infix-symb '(or (and c (not (or (and a (not b)) (and (not a) b)))) (and (not c) (or (and a (not b)) (and (not a) b))))))
;; ->  (!a ^ b ^ !ci)  v  (a ^ !b ^ !ci)  v  (a ^ b ^ ci)  v  (!a ^ !b ^ ci) 
(define (pretty-display expr)
  (map display-with-spaces expr))

(define (display-with-spaces expr)
  (begin (display " ") (display expr) (display " ")))

;; (compact-display '(a ^ b ^ c)) -> a^!b^c
;; (compact-display '!a) -> !a
(define (compact-display expr)
  (if (symbol? expr)
      (display expr)
      (map display expr)))

(define (display-with-bracket expr)
  (begin (display "(")) (display expr) (display ")"))

;; this function start the display without bracket for the outside of complex expressions 
;; (compact-display-bracket '(a ^ !b ^ c)) -> a^!b^c
;; (compact-display-bracket '!a) -> !a
;; (compact-display-bracket '((a ^ b ^ c) v (a ^ !c))) -> (a^b^c)v(a^!c)
;;
;; (compact-display-bracket (dnf-infix-symb '(or (and c (not (or (and a (not b)) (and (not a) b)))) (and (not c) (or (and a (not b)) (and (not a) b))))))
;;    -> (!a^b^!ci)v(a^!b^!ci)v(a^b^ci)v(!a^!b^ci)

(define (compact-display-bracket expr)
  (if (symbol? expr)
      (display expr)
      ;;(begin
      (map bracket-compact-display-bracket expr))) ;; display with bracket for sub-expressions
	;;(newline)))) ;; avoid returning '(#<void> #<void> #<void> .... #<void> #<void>)

(define (bracket-compact-display-bracket expr)
  (if (symbol? expr)
      (display expr)
      (begin
	(display "(")
	(map compact-display-bracket expr) ;; no bracket for inner symbols
	(display ")"))))


(define (pretty-display-condensed expr)
  (if (or (symbol? expr) (symbol? (first expr)));; '!a, a, b, '(a ^ b ^ c), '(a ^ !b) ,...
      (compact-display expr)
      (map compact-display-bracket expr))) ;; '((a ^ b ^ c) v (a ^ !b)) ,...

;; just remove all the trash from displays....
;; (cleaner '(compact-display '(a ^ b ^ c))) 
;; a^b^c
;; (cleaner '(enlight-dnf '(or (and c (not (or (and a (not b)) (and (not a) b)))) (and (not c) (or (and a (not b)) (and (not a) b)))))) -> (!a^!b^c)v(a^b^c)v(a^!b^!c)v(!a^b^!c)
;; and nothing else....
(define (cleaner task)
  (begin
    (eval task ( make-base-namespace ))
    (display "")))
