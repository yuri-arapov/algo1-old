;; coursera.com
;;
;; Design and Analysis of Algorithms I
;;
;; Utilities (Racket).


;; Take up to 'n' first elements of list s.  If list is shorter than given
;; 'n' then use 'dflt' value instead of list elements.
(define (take-upto/default s n dflt)
  (let loop ((s s) (n n) (res '()))
    (if (zero? n) (reverse res)
      (loop (if (null? s) s (cdr s))
            (- n 1)
            (cons (if (null? s) dflt (car s)) res)))))


;; Fold file line-wise.
(define (fold-file proc init path)
  (call-with-input-file
    path
    (lambda (f)
      (let loop ((line (read-line f)) (res init))
        (if (eof-object? line) res
          (loop (read-line f) (proc line res)))))))


;; Wrap given function so that each invocation will print its running time.
(define (make-timed-proc name proc)
  (lambda args
    (let* ((start (current-time))
           (res   (apply proc args))
           (dur   (time-difference (current-time) start)))
      (format #t "(time) ~a: ~0,4F sec~%"
              name (+ (time-second dur) (/ (time-nanosecond dur) 1.0e9)))
      res)))


;; end of file
;; vim: ts=4 sw=4 et
