;; coursera.com
;;
;; Design and Analysis of Algorithms I
;;
;; I-th order select.


(load "utils.scm")


(define (rselect i arr)

  (define (at x) (vector-ref arr x))

  (define (set2 x xx y yy)
    (vector-set! arr x xx)
    (vector-set! arr y yy))

  (define (swap x y) (set2 x (at y) y (at x)))

  (define (random-pivot l r) (+ l (random (- r l -1))))

  ;; Partition subarray elements [l,r] around pivot element (index p).
  ;; Return index of pivot element after partitioning.
  (define (partition l r p)
    (if (<= r l) p
      (begin
        (swap l p)                 ; swap first subarray element and pivot.

        (let loop ((i (1+ l))      ; i - index of first gt. element.
                   (j (1+ l)))     ; j - index of first unpartitioned element.
                                   ; so l+1..i-1 are le. elements, and
                                   ;     i ..j-1 are gt. elements.
          (cond
            ((> j r)               ; partition is done:
             (swap l (1- i))       ;   place pivot between le. and gt. elems.
             (1- i))               ;   and return pivot position.

            ((> (at j) (at l))     ; gt. element,
             (loop i (1+ j)))      ;   nothing special to do.

            (else                  ; le. element, store it next to other
              (swap i j)           ;   le. elements.
              (loop (1+ i) (1+ j))))))))

  (define (rselect l r)
    (if (or (< i l) (> i r)) #f
      (let ((p (partition l r (random-pivot l r))))
        (cond ((< i p) (rselect   l    (1- p)))
              ((> i p) (rselect (1+ p)   r))
              (else    (at i))))))

  (rselect 0 (1- (vector-length arr))))


;; end of file
;; vim: ts=4 sw=4 et
