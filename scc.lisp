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


(defun not-implemented (name)
  "Print 'not yet implemented' message and return NIL."
  (format t "~a~%" (concatenate 'string "FIXME: not implemented yet: " name)))
  ; The 'concatenate' is to remember how to make single string
  ; from the bunch of them.


(defun take-safe (seq n)
  "Take up to n first elements of the sequence depending on its size."
  (subseq seq 0 (min n (length seq))))


(defun const-list (val size)
  "Make list of given size with all elements equal to val."
  (loop for i from 1 to size collect val))


(defun take/default (seq n default)
  "Take first n elments of the sequence seq padding with default value on the
   right if necessary."
  (let ((x (min n (length seq))))
    (nconc (subseq seq 0 x) (const-list default (- n x)))))


(defun read-file-linewise (path proc)
  "Return list of values obtained by applying proc function on each line
   read from file."
  (with-open-file (s path)
    (loop for line = (read-line s nil nil)
          while line
          collect (funcall proc line) into res
          finally (return res))))


(defun line-to-edge (line)
  "Turn line into edge (pair of integers)."
  (multiple-value-bind (from end-of-from) (parse-integer line :junk-allowed t)
    (cons from (parse-integer line :start end-of-from))))


(defun read-edges (file)
  "Return list of edges read from the file."
  (read-file-linewise file #'line-to-edge))


(defun hash-table-keys (ht)
  "Return list of hash table keys."
  (loop for key being the hash-keys of ht
        collect key))


(defun edges-to-vertices (edges)
  "Make list of vertices from list of edges."
  (hash-table-keys
    (reduce
      #'(lambda (ht e)
          (setf (gethash (car e) ht) t)
          (setf (gethash (cdr e) ht) t)
          ht)
      edges
      :initial-value (make-hash-table))))


(defun edges-to-graph (edges &key (reverse-edges nil))
  "Turn list of edges into graph [reversing edges if necessary]."
  (reduce
    #'(lambda (ht e)
        (multiple-value-bind (v w) (if reverse-edges
                                     (values (cdr e) (car e))
                                     (values (car e) (cdr e)))
          (setf (gethash v ht) (cons w (gethash v ht))))
        ht)
    edges
    :initial-value (make-hash-table)))


(defun graph-ref (graph node)
  "Return list of the nodes given node connected to."
  (gethash node graph))


(defun dfs-loop (graph vertices init pre-func acc-func)
  "DFS loop."

  (let ((explored (make-hash-table)))

    (defun exploredp (v)    (gethash v explored))
    (defun set-explored (v) (setf (gethash v explored) t))

    (defun first-unexplored (vv)
      (some #'(lambda (v) (and (not (exploredp v)) v)) vv))

    (defun dfs-iter (v res)
      "Iterative DFS."
      (set-explored v)
      (let ((call-stack (list v))
            (res        res))
        (tagbody
          again
          (if (null call-stack) (go out)
            (let* ((v (car call-stack))
                   (w (first-unexplored (graph-ref graph v))))
              (if w
                (progn
                  (set-explored w)
                  (setf call-stack (cons w call-stack))
                  (go again))
                (progn
                  (setf call-stack (cdr call-stack))
                  (setf res (funcall acc-func v res))
                  (go again)))))
          out)
        res))

    (reduce
      #'(lambda (res v)
          (if (exploredp v) res
            (dfs-iter v (funcall pre-func v res))))
      vertices
      :initial-value init)))


(defun scc (edges)
  "Make list of SCCs -- list of lists of vertices."
  (let* ((_ (format t "pass 1~%"))

         (vv (dfs-loop ; pass 1
               (edges-to-graph edges :reverse-edges t)
               (edges-to-vertices edges)
               '()
               #'(lambda (v acc) acc)
               #'(lambda (v acc) (cons v acc))))

         (_ (format t "pass 2~%"))

         (rr (dfs-loop ; pass 2
               (edges-to-graph edges)
               vv
               '()
               #'(lambda (v acc) (cons '() acc))
               #'(lambda (v acc) (rplaca acc (cons v (car acc)))))))

    rr))


(defun p4-file (file)
  "Read edges from given file, compute SCCs and return list of sizes of
   first 5 largest SCCs."
  (take/default (sort (mapcar #'length (scc (read-edges file))) #'>) 5 0))


(defun p4 ()
  "Solution of Programming Question of Week 4."
  (p4-file "SCC.txt"))


;; end of file
;; vim: ts=4 sw=4 et
