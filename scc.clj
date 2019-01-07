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
;;
;; ~10 minutes, 2G Java heap.


(use '[clojure.string :only (split)])

;; Turn line into list of words (separated by whitespaces in input line).
(defn line->words [line]
  (clojure.string/split line #"\s"))


;; Debugging facility.
(defn debug-print [& args]
  (apply println args)
  (flush))


;; Non-lazy version of map, return list.
(defn my-map [f s]
  (reverse (reduce (fn [res e] (cons (f e) res)) '() s)))


;; Read input file line-wise.
(defn read-file-with [file init line-proc]
  (with-open [rdr (clojure.java.io/reader file)]
    (loop [res init]
      (if-let [line (.readLine rdr)]
        (recur (line-proc line res))
        res))))


;; Return list of lines read from given file.
;; (For testing purposes mostly.)
(defn read-file-lines [file]
  (reverse (read-file-with file '() cons)))


;; Graph edge.
(deftype Edge [^int from ^int to]
  clojure.lang.Counted
    (count [this] 2)
  clojure.lang.Indexed
    (nth [this n] (if (zero? n) from to))
    (nth [this n not-found] (if (zero? n) from to)))


;; Attempt to parse graph's edge efficiently.
(defn parse-edge [line]
  (let [words (.split line " ")
        from  (Integer/valueOf (first words))
        to    (Integer/valueOf (second words))]
    (Edge. from to)))


;; Read graph edges from the file.
(defn read-edges [file]
  (debug-print "read-edges" file)
  (read-file-with
    file
    []
    (fn [line acc] (conj acc (parse-edge line)))))


;; List of (v w) edges -> {v (w)} map.
(defn edges->graph [edges]
  (debug-print "edges->graph")
  (persistent!
    (reduce
      (fn [res [v w]] (assoc! res v (cons w (get res v '()))))
      (transient {})
      edges)))


;; Sama as (edges->graph ...), but all the edges reversed.
(defn edges->rev-graph [edges]
  (debug-print "edges->rev-graph")
  (persistent!
    (reduce
      (fn [res [w v]] (assoc! res v (cons w (get res v '()))))
      (transient {})
      edges)))


;; Get list of connections of given vertex.
(defn graph-ref [g v] (get g v '()))


;; Turn list of edges into set of vertices (in arbitrary order)
;; (we're good because set is a sequence in Clojure).
(defn edges->vertices [edges]
  (debug-print "edges->vertices")
  (persistent!
    (reduce
      (fn [res [v w]] (conj! (conj! res v) w))
      (transient #{})
      edges)))


;; DFS outer loop.
(defn dfs-loop [graph vertices init pre-dfs-fn dfs-accum-fn]

  (debug-print "dfs-loop")

  (let [explored (atom #{})
        counter  (atom 0)]

    (defn explored? [v] (get @explored v))
    (defn set-explored! [v] (reset! explored (conj @explored v)))

    (defn first-unexplored [vv] (some #(if-not (explored? %) %) vv))

    ;; DFS: iterative.
    (defn dfs-iter [v res]
      (set-explored! v)
      (loop [callstack (list v)
             res       res]

;;        (reset! counter (+ 1 @counter))
;;        (if (zero? (rem @counter 10000))
;;          (debug-print @counter))

        (if (empty? callstack) res
          (let [v         (first callstack)
                callstack (rest callstack)
                w         (first-unexplored (graph-ref graph v))]
            (if (not w)
              (recur callstack (dfs-accum-fn v res))
              (recur (do
                       (set-explored! w)
                       (cons w (cons v callstack)))
                     res))))))

    (reduce (fn [res v]
              (if (explored? v) res
                (dfs-iter v (pre-dfs-fn v res))))
            init
            vertices)))


;; Split graph defined as sequence of edges into sequence of strongly connected
;; components (SCC).
(defn scc [edges]
  (let [
        vv (reverse (persistent! (dfs-loop  ; pass 1
             (edges->rev-graph edges)
             (edges->vertices edges)
             (transient [])
             (fn [v acc] acc)
             (fn [v acc] (conj! acc v)))))

        _  (do (debug-print "pass 1 ended") nil)

        rr (dfs-loop                        ; pass 2
             (edges->graph edges)
             vv
             '()
             (fn [v acc] (cons '() acc))
             (fn [v acc] (cons (cons v (first acc)) (rest acc))))]

    rr))


;; Some test graph
(def test-edges
  '((8 7) (7 6) (6 8) (7 9) (1 9) (9 4) (4 1) (4 3) (3 5)
    (5 2) (6 1) (1 2) (6 10) (10 8) (10 6) (2 3)))


;; Guess what.
(defn scc-test [] (scc test-edges))


;; Programming question of Algo class I, week 4.
(defn p4
  ([]     (p4 "SCC.txt"))
  ([file] (take 5 (sort > (my-map count (scc (read-edges file)))))))


;; end of file
;; vim: ts=2 sw=2
