
;; (compute-Ck-plus1 0)
;; '(or (and B0 B1 C0) (and B0 (not B1) (not C0)) (and (not B0) B1 (not C0)) (and (not B0) (not B1) C0))
(define (compute-Ck-plus1 k)
  {Bk <+ (string->symbol (string-append "B" (number->string k)))}
  {Bkp1 <+  (string->symbol (string-append "B" (number->string {k + 1})))} ;; Bk+1
  {Ck <+ (string->symbol (string-append "C" (number->string k)))}
  (minimal-dnf  `{,Bk ⊕ ,Bkp1 ⊕ ,Ck}))


(define (compute-carries n)

  (for ( {k <+ 0}  {k <= n}  {k <- {k + 1}} )

       { Ckp1 <+ (compute-Ck-plus1 k) }
       (display-nl Ckp1)
       
       ))
