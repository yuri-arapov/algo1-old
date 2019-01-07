;; coursera.com
;;
;; Design and Analysis of Algorithms I
;;
;; Quick sort.


(load "utils.scm")


;; True if for each argument from 'from' to 'to' (including) function 'fn'
;; return true.
(define (every-time? fn from to)
  (if (> from to) #t
    (if (not (fn from)) #f
      (every-time? fn (1+ from) to))))


;; True if vector 'arr' is sorted in increasing order.
(define (sorted? arr)
  (define (at x) (vector-ref arr x))
  (every-time?
    (lambda (n) (< (at n) (at (1+ n))))
    0
    (- (vector-length arr) 2)))


;; Quick Sort it is ('arr' is a vector, inplace sorting).
;; 'pivot-fn' is a 3-argument function to compute pivot: (pivot-fn arr l r).
(define (quick-sort-int! pivot-fn arr)

  (define (at x) (vector-ref arr x))

  (define (set2 x xx y yy)
    (vector-set! arr x xx)
    (vector-set! arr y yy))

  (define (swap x y) (set2 x (at y) y (at x)))

  (let ((comparison-counter 0))      ; to solve programming tasks.

    (define (update-comparison-counter n)
      (set! comparison-counter (+ comparison-counter n)))

    (define (partition l r)
      (if (<= r l) arr               ; nothing to partition, return arr.
        (begin
          (update-comparison-counter (- r l))
          (swap l (pivot-fn arr l r)); choose pivot and place it into 1st pos.
          (let loop ((i (1+ l))      ; i - index of first gt. element.
                     (j (1+ l)))     ; j - index of first unpartitioned element.
                                     ; so l+1..i-1 are le. elements, and
                                     ;     i ..j-1 are gt. elements.
            (cond
              ((> j r)               ; partition is done:
               (swap l (1- i))       ;   place pivot between le. and gt. elems.
               (partition l (- i 2)) ;   partition le. elements
               (partition i r))      ;   partition gt. elements.

              ((> (at j) (at l))     ; gt. element,
               (loop i (1+ j)))      ;   nothing special to do.

              (else                  ; le. element, store it next to other
                (swap i j)           ;   le. elements.
                (loop (1+ i) (1+ j))))))))

    (let ((res (partition 0 (1- (vector-length arr)))))
      (values res comparison-counter))))


;; Return left/right element as a pivot.
(define (pivot-l _ l r) l)
(define (pivot-r _ l r) r)


;; Return mid3 element as a pivot.
(define (pivot-mid3 arr l r)
  (define (at x) (vector-ref arr x))
  (let ((m (+ l (quotient (- r l) 2))))
    (cond ((<= (at l) (at m) (at r)) m)
          ((<= (at r) (at m) (at l)) m)
          ((<= (at m) (at l) (at r)) l)
          ((<= (at r) (at l) (at m)) l)
          ((<= (at m) (at r) (at l)) r)
          ((<= (at l) (at r) (at m)) r)
          (else (error "pivot-mid3" l r)))))


;; Random pivot.
(define (random-pivot _ l r) (+ l (random (- r l -1))))


;; Quick sort with random pivot.
(define (quick-sort! arr) (quick-sort-int! random-pivot arr))


;; Sort data from file, return number of comparison needed for sorting.
(define (quick-sort-file pivot-fn file)
  (let-values
    (((sorted-array comparison-counter)
      (quick-sort-int!
        pivot-fn
        (list->vector (read-file-with file string->number)))))
    comparison-counter))


;; Programming task #2.
(define (p2)
  (map (rcurry quick-sort-file "QuickSort.txt")
       (list pivot-l pivot-r pivot-mid3)))


;; end of file
;; vim: ts=4 sw=4 et
