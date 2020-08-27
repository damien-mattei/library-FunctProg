;; useful macros 

;; author: Damien Mattei

;; "if" "then" without "else"
;;  (if-t (= 1 1) 'good) -> 'good
;;  (if-t (= 1 1) 'good 'bad) -> 'bad
;;  (if-t (= 1 1) 'good 'bad 'bof) -> 'bof
;; > (if-t (= 1 1) (display-nl 'good) (display-nl 'bad) (display-nl 'bof))
;; good
;; bad
;; bof
;; > (if-t (= 1 0) (display-nl 'good) (display-nl 'bad) (display-nl 'bof)) ->  '()
(define-syntax if-t
  (syntax-rules ()
    ((_ tst ev)  (if tst ev '()))
    ((_ tst ev ...)  (if tst (begin ev ...) '()))))

;; then and else do as begin-ners ;-)
;; (if-t #t (then "hello" "bye"))
;; "bye"
(define-syntax then
  (syntax-rules ()
    ((_ ev)  ev)
    ((_ ev ...) (begin ev ...))))


;; then and else do as BEGINners ;-)
(define-syntax then-block
  (syntax-rules ()
    ((_ ev)  ev)
    ((_ ev ...) (begin ev ...))))

(define-syntax else-block
  (syntax-rules ()
    ((_ ev)  ev)
    ((_ ev ...) (begin ev ...))))

