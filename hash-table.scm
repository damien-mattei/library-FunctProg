
;; delete all the elements of a hash table but not the hash table itself (will be done by garbage collector)

(define (hash-table-clear! ht) (map (lambda (key) (hash-table-delete! ht key))
				    (hash-table-keys ht)))

