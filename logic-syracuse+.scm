
;; (compute-Ck-plus1 0)
;; '(or (and B0 B1 C0) (and B0 (not B1) (not C0)) (and (not B0) B1 (not C0)) (and (not B0) (not B1) C0))
(define (compute-Ck-plus1 k)
  {Bk <+ (string->symbol (string-append "B" (number->string k)))}
  ;;{Bkp1 <+  (string->symbol (string-append "B" (number->string {k + 1})))} ;; Bk+1
  {Bkm1 <+  (string->symbol (string-append "B" (number->string {k - 1})))} ;; Bk-1
  {Ck <+ (string->symbol (string-append "C" (number->string k)))}
  (minimal-dnf  `{{,Bk · ,Bkm1} ⊕ {,Ck · {,Bk ⊕ ,Bkm1}}})) ;; the formula of the ouput Carry of the k-th digits for the 3x+1 Collatz computation 


;; return 'Ck
(define (create-Ck-symb k)
  (string->symbol (string-append "C" (number->string k))))


;; compute the k-th first carries of 3x+1
(define (compute-carries n)

  (define k 1)
  (define Ck (compute-Ck-plus1 0))
  {Ck-symb <+ (create-Ck-symb k)}
  (display-nl (format #f "~a = ~a" Ck-symb Ck))
  {Ck-infix <+ (prefix->infix-symb Ck)}
  (display-nl (format #f "~a = ~a" Ck-symb Ck-infix))

  {Ck <- (replace Ck 'B-1 'F)} ;; 2x shift left the number and insert a 0 at rightmost position
  {Ck <- (replace Ck 'C0 'T)} ;; initial carry set
  {Ck <- (replace Ck 'B0 'T)} ;; because number b is odd
  
  (display-nl (format #f "~a = ~a" Ck-symb Ck))
  {Ck <- (minimal-dnf Ck)}
  (display-nl (format #f "~a = ~a" Ck-symb Ck))
  {Ck-infix <- (prefix->infix-symb Ck)}
  (display-nl (format #f "~a = ~a" Ck-symb Ck-infix))

  
  (repeat

   (newline)
   {Ckp1 <+ (compute-Ck-plus1 k)} ;; note: instead we can substitute symbols
   {Ckp1 <- (replace Ckp1 'B0 'T)} ;; because number b is odd
   {Ckp1-symb <+ (create-Ck-symb {k + 1})}
   ;;(display-nl (format #f "~a = ~a" Ckp1-symb Ckp1))
   {Ckp1 <- (replace Ckp1 Ck-symb Ck)} ;; substitute in Ck+1 ,Ck symbol by is previous computation
   ;;(display-nl (format #f "~a = ~a" Ckp1-symb Ckp1))
   {Ckp1 <- (minimal-dnf Ckp1)}
   ;;(display-nl (format #f "~a = ~a" Ckp1-symb Ckp1))
   {Ckp1-infix <+ (prefix->infix-symb Ckp1)}
   (display-nl (format #f "~a = ~a" Ckp1-symb Ckp1-infix))
   
   
   {Ck <- Ckp1}
   {Ck-symb <- Ckp1-symb}
   {k <- {k + 1}}
   
   until {k >= n}))
