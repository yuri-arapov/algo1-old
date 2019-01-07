;; coursera.com
;;
;; Design and Analysis of Algorithms I
;;
;; Utilities
;;
;;   large-enough?      true if given list is at least of given length long
;;
;;   take-upto          take first n elements of the list (or less if no such
;;                      many elements)
;;
;;   shuffle            shuffle the list randomly
;;
;;   line->words-pred   turn string into list of words using predicate that
;;                      checks for word characters
;;
;;   line->words        turn string into list of words (separated by white
;;                      spaces in input string)
;;
;;   line->numbers      turn string into list of numbers
;;
;;   line->numbers/sep  turn string into list of numbers, separator specified
;;                      explicitly
;;
;;   make-timed-proc    wrap function into some extra code so it will print
;;                      its running time right before returning result


;; True if s contains at least len elements.
;; FIXME: move to reusable code (~/scheme).
(define (large-enough? s len)
  (cond ((zero? len) #t)
        ((null? s)   #f)
        (else        (large-enough? (cdr s) (1- len)))))


;; Take up to len first elements of list s (or less if s is not large enough).
;; FIXME: move to reusable code (~/scheme).
(define (take-upto s len)
  (let loop ((s s) (len len) (res '()))
    (if (or (null? s) (zero? len)) (reverse res)
      (loop (cdr s) (1- len) (cons (car s) res)))))


;; Take up to 'len' first elements of list s.  If list is shorter than given 
;; 'len' then use 'default' value instead of list elements.
;; FIXME: move to reusable code (~/scheme).
(define (take-upto/default s len default)
  (define (next s) (if (null? s) s (cdr s)))
  (define (head s) (if (null? s) default (car s)))
  (let loop ((s s) (len len) (res '()))
    (if (zero? len) (reverse res)
      (loop (next s) (1- len) (cons (head s) res)))))


;; Shuffle list.
;; FIXME: move to reusable code (~/scheme).
(define (shuffle s)
  (let ((len (length s)))
    (map cdr (sort (map (lambda (e) (cons (random len) e)) s) (with < car)))))


;; Turn line into list of words so that each word's character c would cause
;; (word-char? c) function return True.
(define (line->words-pred word-char? line)

  (define (put-word word words)
    (if (null? word) words
      (cons (list->string (reverse word)) words)))

  (define (put-char line word) (cons (car line) word))

  (let loop ((line (string->list line))
             (word '())
             (words '()))
    (cond ((null? line)
           (reverse (put-word word words)))
          ((word-char? (car line))
           (loop (cdr line) (put-char line word) words))
          (else
            (loop (cdr line) '() (put-word word words))))))


;; Split line into words; words separator is any number of blank characters.
(define (line->words line)
  (line->words-pred (lambda (c) (> (char->integer c) 32)) line))


;; Turn string into list of numbers.
(define (line->numbers line)
  (map string->number (line->words line)))


;; Faster version of line->numbers.
(define (line->numbers/sep separator line)
  (map string->number
       (remove (curry string=? "")
               (string-split line separator))))


;; Wrap given function so that each invocation will print its running time.
(define (make-timed-proc name proc)
  (lambda args
    (let* ((start (current-time))
           (res   (apply proc args))
           (dur   (time-difference (current-time) start)))
      (format #t "(time) ~a: ~,4f sec~%"
              name (+ (time-second dur) (/ (time-nanosecond dur) 1.0e9)))
      res)))


(define (parse-integer s start)
  (let ((len  (string-length s))
        (spce (char->integer #\space))
        (zero (char->integer #\0))
        (nine (char->integer #\9)))

    (define (digit? x) (<= zero x nine))
    (define (space? x) (<= x spce))

    (let loop ((n 0) (count 0) (i start))
      (let ((x (if (= i len) spce (char->integer (string-ref s i)))))
        (cond ((and (space? x) (zero? count) (< i len))
               (loop n count (1+ i)))
              ((not (digit? x))
               (if (zero? count) (error "parse-number: not a number")
                 (values n i)))
              (else
                (loop (+ (- x zero) (* n 10)) (1+ count) (1+ i))))))))


(define (blank-line? s start)
  (let ((len (string-length s)))
    (let loop ((i start))
      (cond ((= i len) #t)
            ((char<=? (string-ref s i) #\space) (loop (1+ i)))
            (else #f)))))


;; end of file
;; vim: ts=4 sw=4 et
