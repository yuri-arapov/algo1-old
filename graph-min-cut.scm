;; coursera.com
;;
;; Design and Analysis of Algorithms I
;;
;; Programming Question - 3
;; Question 1
;; NOTE - This dataset has been changed (as of Monday, March 26) due to some
;; errors in the previous dataset. So if you downloaded the file earlier,
;; please, download it again, and run your code on this new data set.
;;
;; Download the text file kargerAdj.txt.
;;
;; The file contains the adjacency list representation of a simple undirected
;; graph. There are 40 vertices labeled 1 to 40. The first column in the file
;; represents the vertex label, and the particular row (other entries except
;; the first column) tells all the vertices that the vertex is adjacent to. So
;; for example, the 6th row looks liks : "6 29 32 37 27 16". This just means
;; that the vertex with label 6 is adjacent to (i.e., shares an edge with) the
;; vertices with labels 29, 32, 37, 27 and 16.
;;
;; Your task is to code up and run the randomized contraction algorithm for the
;; min cut problem and use it on the above graph to compute the min cut. (HINT:
;; Note that you'll have to figure out an implementation of edge contractions.
;; Initially, you might want to do this naively, creating a new graph from the
;; old every time there's an edge contraction. But you also think about more
;; efficient implementations.) (WARNING: As per the video lectures, please make
;; sure to run the algorithm many times with different random seeds, and
;; remember the smallest cut that you ever find). Write your numeric answer in
;; the space provided. So e.g., if your answer is 5, just type 5 in the space
;; provided.
;;
;; Answer: 3


(load "utils.scm")


(use-modules (srfi srfi-69))    ;; hash tables


;; Read graph from file.
(define (read-graph fname)
  (read-file-with fname line->numbers))


;; Turn graph into V-list: list of elements such as each element is a list
;; where first element is vertex and all the rest elements are edges.
(define (graph->v-list g)
  (reverse
    (fold
      (lambda (ls res)
        (cons
          (cons (car ls) (map (lambda (v) (cons (car ls) v)) (cdr ls)))
          res))
      '()
      g)))


;; Turn graph into list of edges.
(define (graph->e-list g)
  (reverse
    (fold
      (lambda (ls res)
        (let ((v1 (car ls)))
          (fold
            (lambda (v res)
              (if (not (< v1 v)) res
                (cons (cons v1 v) res)))
            res
            (cdr ls))))
      '()
      g)))


;; Read task graph from the file.
(define (task-graph)
  (read-graph "kargerAdj.txt"))
  ;;(read-graph "kargerMinCut.txt"))


;; Random graph cut.
(define (random-cut g n)

;;  (format #t "random-cut ~a\n" n)

  (let* ((V (alist->hash-table (graph->v-list g) =))
         (n (hash-table-size V))
         (E (list->vector (graph->e-list g)))
         (R 0)) ; number of edges removed from E.

    (define (m)             (- (vector-length E) R))
    (define (e-ref r)       (vector-ref E r))
    (define (e-set! r val)  (vector-set! E r val))

    (define (e-replace old new)
      (dotimes (i (m))
        (let ((e (e-ref i)))
          (cond ((= old (car e)) (e-set! i (cons new (cdr e))))
                ((= old (cdr e)) (e-set! i (cons (car e) new)))
                (else #f)))))

    ;; Remove loop-edges from the list of edges.
    (define (e-remove-loops)
      (let loop ((i 0))
        (if (= i (m)) #f
          (let ((e (e-ref i)))
            (if (not (= (car e) (cdr e))) (loop (1+ i))
              (begin
                (e-set! i (e-ref (1- (m)))) ; move last edge to i-th pos.
                (set! R (1+ R))             ; increase number of removed edges.
                (loop i)))))))

    (dotimes (_ (- n 2)) ; keep two last vertices.
      (let* ((re (e-ref (random (m))))  ; random edge.
             (i1 (car re))              ; indeces of vertices that
             (i2 (cdr re)))             ;   make this random edge up.
        (e-replace i1 i2)
        (e-remove-loops)))
    (m)))


;; Solution of programming task 3.
(define (p3)
  (let* ((g (task-graph))
         (n (length g)))
    (apply min (map (lambda (n) (random-cut g n)) (iota (* n 5))))))


;; end of file
;; vim: ts=4 sw=4 et
