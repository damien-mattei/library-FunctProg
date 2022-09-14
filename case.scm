;; from R7RS-small
;; original version with 'begin
;; (define-syntax case
;;   (syntax-rules (else =>)
;;     ((case (key ...) clauses ...)
;;      (let ((atom-key (key ...)))
;;        (case atom-key clauses ...)))
;;     ((case key (else => result))
;;      (result key))
;;     ((case key
;;        (else result1 result2 ...))
;;      (begin result1 result2 ...))
;;     ((case key
;;        ((atoms ...) result1 result2 ...))
;;      (if (memv key ’(atoms ...))
;;          (begin result1 result2 ...)))
;;     ((case key
;;        ((atoms ...) => result))
;;      (if (memv key ’(atoms ...))
;;          (result key)))
;;     ((case key
;;        ((atoms ...) => result)
;;        clause clauses ...)
;;      (if (memv key ’(atoms ...))
;;          (result key)
;;          (case key clause clauses ...)))
;;     ((case key
;;        ((atoms ...) result1 result2 ...)
;;        clause clauses ...)
;;      (if (memv key ’(atoms ...))
;;          (begin result1 result2 ...)
;;          (case key clause clauses ...)))))

;; the modified version replace 'begin by 'let ()
(define-syntax case
  (syntax-rules (else =>)
    ((case (key ...) clauses ...)
     (let ((atom-key (key ...)))
       (case atom-key clauses ...)))
    ((case key (else => result))
     (result key))
    ((case key
       (else result1 result2 ...))
     (let () result1 result2 ...))
    ((case key
       ((atoms ...) result1 result2 ...))
     (if (memv key ’(atoms ...))
         (let () result1 result2 ...)))
    ((case key
       ((atoms ...) => result))
     (if (memv key ’(atoms ...))
         (result key)))
    ((case key
       ((atoms ...) => result)
       clause clauses ...)
     (if (memv key ’(atoms ...))
         (result key)
         (case key clause clauses ...)))
    ((case key
       ((atoms ...) result1 result2 ...)
       clause clauses ...)
     (if (memv key ’(atoms ...))
         (let () result1 result2 ...)
         (case key clause clauses ...)))))

