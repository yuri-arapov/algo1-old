

(define (merge-sort v)
  (let ((n (length v)))
    (if (or (= n 1) (= n 0)) v
        (let* ((an (quotient n 2))
               (a (take v an))
               (b (drop v an))
               (as (merge-sort a))
               (bs (merge-sort b)))
          (let loop ((i as) (j bs) (res '()))
            (cond ((and (null? i) (null? j)) (reverse res))
                  ((null? i) (loop i       (cdr j) (cons (car j) res)))
                  ((null? j) (loop (cdr i) j       (cons (car i) res)))
                  ((< (car i) (car j)) (loop (cdr i) j (cons (car i) res)))
                  (else                (loop i       (cdr j) (cons (car j) res)))))))))
