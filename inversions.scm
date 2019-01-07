;; coursera.com
;;
;; Design and Analysis of Algorithms I
;;
;; Count inversions
;;
;; Programming task #1:
;;   count inversion of numbers in the file IntegerArray.txt.
;;
;; Answer: 2407905288


;; Count inversions of elements in the list.  The '<' operator must be
;; applicable to list elements.
;; O(n log(n)).
(define (count-inversion s)

  ;; True if s is a list of single element.
  (define (single? s) (and (not (null? s)) (null? (cdr s))))

  ;; The b-len is a length of b list so there's no need to compute it.
  (define (merge-and-count-split b b-len c)

    ;; (put ...) is just a shortcut.
    (define (put from to) (cons (car from) to))

    ;; Merge loop: form resultant list (d) and count number of inversions (z).
    (let loop ((b b) (b-len b-len) (c c) (z 0) (d '()))
      (cond ((and (null? b) (null? c)) (values z (reverse d)))
            ((null? c) (loop (cdr b) b-len      c  z (put b d)))
            ((null? b) (loop      b  b-len (cdr c) z (put c d)))
            (else ;; both b and c are not null
              (if (< (car b) (car c))
                (loop (cdr b) (1- b-len)      c     z        (put b d))
                (loop      b      b-len  (cdr c) (+ z b-len) (put c d)))))))

  (define (sort-and-count s)
    (if (or (null? s) (single? s)) (values 0 s)
      (let ((b-len (quotient (length s) 2))) ;; length of left sub-list
        (let*-values (((x b) (sort-and-count (take s b-len)))
                      ((y c) (sort-and-count (drop s b-len)))
                      ((z d) (merge-and-count-split b b-len c)))
          (values (+ x y z) d)))))

  (let ((res (sort-and-count s)))
    res))


;; Count inversions in the file containing integer number (one number per line,
;; no blank lines).
(define (count-inversion-file file)
  (count-inversion (read-file-with file string->number)))


;; Solve programming task #1.
(define (p1)
  (count-inversion-file "IntegerArray.txt"))


;; end of file
;; vim: ts=4 sw=4 et
