;; coursera.com
;;
;; Design and Analysis of Algorithms I
;;
;; Closest pair.


(load "utils.scm")


;; Point: constructor and accessors.
(define (mk-p x y) (cons x y))
(define (x p) (car p))
(define (y p) (cdr p))


;; Make list of random points of given length.
;; Note: all the x- and y-coordinates are distinct.
(define (random-points len)
  (map mk-p (shuffle (iota len)) (shuffle (iota len))))


(define test-data
  '((-1 .  6)
    ( 5 .  0)
    (12 .  7)
    ( 7 .  8)
    ( 4 .  5)
    ( 6 . 13)))


;; Compute distance between points p1 and p2.
(define (dist p1 p2)
  (define (sqr n) (* n n))
  (define (sqr-diff a b) (sqr (- a b)))
  (sqrt (+ (sqr-diff (x p1) (x p2)) (sqr-diff (y p1) (y p2)))))


;; Closest pair result: constructor, accessors, utils.
(define (mk-res dist p1 p2) (list dist p1 p2))
(define (res-dist res)      (car res))
(define (null-res? res)     (not (car res)))
(define (p-p->res p1 p2)    (mk-res (dist p1 p2) p1 p2))


(define null-res (mk-res #f #f #f))


;; Return better result out of two results r1 and r2.
(define (better-res r1 r2)
  (cond ((null-res? r2)                  r1)
        ((null-res? r1)                  r2)
        ((< (res-dist r1) (res-dist r2)) r1)
        (else                            r2)))


;; Find closest pair: burte force, O(n^2).
;; Return (distance p1 p2) list.
(define (closest-pair-brute-force s)
  (let loop ((s s) (res null-res))
    (if (not (large-enough? s 2)) res
      (loop (cdr s)
            (fold (lambda (p res) (better-res res (p-p->res (car s) p)))
                  res
                  (cdr s))))))


;; Find closest pair of points amongst list s.  O(n log(n)).
;; Return (distance p1 p2) list.
;; IMPORTANT: ALL the points are supposed to have DISTINCT x-coordinate.
(define (closest-pair s)

  (define (improve-res ls res)
    (if (not (large-enough? ls 2)) res
      (fold (lambda (p res) (better-res res (p-p->res (car ls) p)))
            res
            (cdr ls))))

  (define (closest-split-pair Py x_ res)
    (let* ((d  (res-dist res))
           (Sy (filter (lambda (p) (<= (abs (- x_ (x p))) d)) Py)))
      (let loop ((Sy Sy) (res res))
        (if (not (large-enough? Sy 2)) res
          (loop (cdr Sy) (improve-res (take-upto Sy 8) res))))))

  (define (closest-pair Px Py)
    (let ((len (length Px)))
      (if (< len 4) (closest-pair-brute-force Px)
        (let*
          ((half-len (quotient len 2))
           (Qx (take Px half-len))                      ;; left half,  x-sorted
           (Rx (drop Px half-len))                      ;; right half, y-sorted
           (x_ (x (car Rx)))                            ;; partition
           (Qy (filter (lambda (p) (<  (x p) x_)) Py))  ;; left half,  y-sorted
           (Ry (filter (lambda (p) (>= (x p) x_)) Py))) ;; right half, y-sorted
          (closest-split-pair Py x_
            (better-res (closest-pair Qx Qy)
                        (closest-pair Rx Ry)))))))

  (closest-pair
    (sort s (with < x))
    (sort s (with < y))))


;; end of file
;; vim: ts=4 sw=4 et
