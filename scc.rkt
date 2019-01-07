;; coursera.com
;;
;; Design and Analysis of Algorithms I
;;
;; Programming Question-4
;; Question 1
;;
;; Download the SCC.txt text file.
;;
;; The file contains the edges of a directed graph. Vertices are labeled as
;; positive integers from 1 to 875714. Every row indicates an edge, the vertex
;; label in first column is the tail and the vertex label in second column is
;; the head (recall the graph is directed, and the edges are directed from the
;; first column vertex to the second column vertex). So for example, the 11th
;; row looks liks : "2 47646". This just means that the vertex with label 2 has
;; an outgoing edge to the vertex with label 47646
;;
;; Your task is to code up the algorithm from the video lectures for computing
;; strongly connected components (SCCs), and to run this algorithm on the given
;; graph.
;;
;; Output Format: You should output the sizes of the 5 largest SCCs in the
;; given graph, in decreasing order of sizes, separated by commas (avoid any
;; spaces). So if your algorithm computes the sizes of the five largest SCCs to
;; be 500, 400, 300, 200 and 100, then your answer should be
;; "500,400,300,200,100". If your algorithm finds less than 5 SCCs, then write
;; 0 for the remaining terms. Thus, if your algorithm computes only 3 SCCs
;; whose sizes are 400, 300, and 100, then your answer should be
;; "400,300,100,0,0".
;;
;; Answer: 434821 968 459 313 211


(require srfi/1)            ;; list function: (any ...)
(require srfi/19)           ;; (current-time ...)
(require srfi/48)           ;; (format ...)


(load "utils.rkt")


;; Split line into list of numbers (space-separated).
(define (line->numbers/re-sep re line)
  (map string->number
       (filter (lambda (s) (not (string=? "" s))) (regexp-split re line))))


;; Read edges from file.
;; Each line is an arc (directed edge) represented as pair of nodes (numbers).
;; Return list of edges.
(define (read-edges fname)
  (let ((re (regexp " ")))
    (reverse
      (fold-file
        (lambda (line res) (cons (line->numbers/re-sep re line) res))
        '()
        fname))))
(define read-edges (make-timed-proc "read-edges" read-edges))


;; Turn list of edges into graph: set of vertices and their connections.
(define (edges->graph edges #:reverse-edges (reverse-edges #f))
  (fold
    (lambda (e ht)
      (let-values (((v w) (if reverse-edges
                            (values (cadr e) (car e))
                            (values (car e) (cadr e)))))
        (hash-update!
          ht
          v
          (lambda (heads) (cons w heads))
          '())
        ht))
    (make-hasheq)
    edges))


;; Get list of connections of given vertex.
(define (graph-ref g r) (hash-ref g r '()))


;; Turn list of edges into list of graph vertices.
(define (edges->vertices edges)
  (hash-keys
    (fold
      (lambda (e ht)
        (hash-set! ht (car e)  #t)
        (hash-set! ht (cadr e) #t)
        ht)
      (make-hasheq)
      edges)))


;; Main DFS loop.  Run DFS for each unexplored vertex from input list and
;; collect DFS's result via accumulating functions.
;;
;; graph        -- enumerator of the vertices (tails) bound to other vertices
;;                 (heads)
;; vertices     -- list of vertices
;; init         -- initial value of accumolator
;; pre-dfs-fn   -- (pre-df-fn v old-accum) -> new-accum
;; dfs-accum-fn -- (dfs-accum-fn v old-accum) -> new-accum
(define (dfs-loop graph vertices init pre-dfs-fn dfs-accum-fn)

  (let ((explored (make-hasheq)))

    (define (explored? v)     (hash-ref  explored v #f))
    (define (set-explored! v) (hash-set! explored v #t))

    (define (first-unexplored vv)
      (any (lambda (v) (and (not (explored? v)) v)) vv))

    ;; Iterative DFS.
    (define (dfs-iter v res)
      (set-explored! v)
      (let loop ((callstack (list v))
                 (res res))
        (if (null? callstack) res
          (let* ((v         (car callstack))
                 (callstack (cdr callstack))
                 (w         (first-unexplored (graph-ref graph v))))
            (if (not w)
              (loop callstack (dfs-accum-fn v res))
              (loop (begin
                      (set-explored! w)
                      (cons* w v callstack))
                    res))))))

    ; beginning of dfs-loop
    (fold (lambda (v res)
            (if (explored? v) res
              (dfs-iter v (pre-dfs-fn v res))))
          init
          vertices)))
(define dfs-loop (make-timed-proc "dfs-loop" dfs-loop))


;; Run SCC on given list of [directed] edges.
;; Return list of lists, where each one represent some SCC of the graph.
(define (scc edges)
  (let* ((vv (dfs-loop ; pass 1
               (edges->graph edges #:reverse-edges #t)
               (edges->vertices edges)
               '()
               (lambda (v acc) acc)
               (lambda (v acc) (cons v acc))))

         (rr (dfs-loop ; pass 2
               (edges->graph edges)
               vv
               '()
               (lambda (v acc) (cons '() acc))
               (lambda (v acc) (cons (cons v (car acc)) (cdr acc))))))

    rr))
(define scc (make-timed-proc "scc" scc))


;; Some test graph
(define test-edges
  '((8 7) (7 6) (6 8) (7 9) (1 9) (9 4) (4 1) (4 3) (3 5)
    (5 2) (6 1) (1 2) (6 10) (10 8) (10 6) (2 3)))


;; Guess what.
(define (test)
  (scc test-edges))


;; Return sizes of 5 largest SCC of the graph stored in given file.
(define (p4-file file)
  (take-upto/default (sort (map length (scc (read-edges file))) >) 5 0))


;; Solve programming question of week 4.
(define (p4)
  (p4-file "SCC.txt"))


;; end of file
;; vim: ts=4 sw=4 et
