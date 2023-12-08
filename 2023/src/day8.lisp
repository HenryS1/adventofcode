(defpackage :day8
  (:use 
   :cl 
   :iterate 
   :alexandria 
   :anaphora 
   :pears
   :metabang-bind)
  (:export
   :parse-node
   :make-node
   :follow-directions
   :parse-node-map
   :find-starting-points
   :follow-multiple-paths-simultaneously
   :node-id))

(in-package :day8)

(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defstruct node id left right)

(defun parse-node ()
  (sequential (id (manyn #'alphanumericp 3))
              (_ (ignore-whitespace))
              (_ (char1 #\=))
              (_ (ignore-whitespace))
              (_ (char1 #\())
              (left (manyn #'alphanumericp 3))
              (_ (char1 #\,))
              (_ (ignore-whitespace))
              (right (manyn #'alphanumericp 3))
              (_ (char1 #\)))
              (make-node :id id :left left :right right)))

(defstruct node-map directions nodes)

(defun make-graph (nodes)
  (loop with graph = (make-hash-table :test 'equal)
        for node in nodes
        do (setf (gethash (node-id node) graph) node)
        finally (return graph)))

(defun parse-node-map ()
  (sequential (directions (many1 #'alphanumericp))
              (_ (many1 #'newlinep))
              (nodes (sep-by (parse-node) (many1 #'newlinep)))
              (make-node-map :directions directions :nodes (make-graph nodes))))

(defun follow-directions (node-map)
  (loop with directions = (node-map-directions node-map)
        with graph = (node-map-nodes node-map)
        with start-node = (gethash "AAA" graph)
        for i = 0 then (mod (+ i 1) (length directions))
        for step-count from 1
        for current-direction = (aref directions i)
        for current-node = (if (char= current-direction #\L)
                               (gethash (node-left start-node) graph)
                               (gethash (node-right start-node) graph)) 
          then (if (char= current-direction #\L) 
                   (gethash (node-left current-node) graph)
                   (gethash (node-right current-node) graph))
        until (string= (node-id current-node) "ZZZ")
        finally (return step-count)))

(defun part1 ()
  (let ((node-map (parse-file "input8" (parse-node-map))))
    (follow-directions node-map)))

(defun find-starting-points (node-map)
  (loop for node being the hash-values of (node-map-nodes node-map) 
        for id = (node-id node)
        when (char= (aref id 2) #\A)
          collect node into starting-points
        finally (return (coerce starting-points 'vector))))

(defun find-end-of-path (start node-map)
  (loop with directions = (node-map-directions node-map)
        with graph = (node-map-nodes node-map)
        with start-node = (gethash start graph)
        for i = 0 then (mod (+ i 1) (length directions))
        for step-count from 1
        for current-direction = (aref directions i)
        for current-node = (if (char= current-direction #\L)
                               (gethash (node-left start-node) graph)
                               (gethash (node-right start-node) graph)) 
          then (if (char= current-direction #\L) 
                   (gethash (node-left current-node) graph)
                   (gethash (node-right current-node) graph))
        until (char= (aref (node-id current-node) 2) #\Z)
        finally (return step-count)))

(defun find-path-ends-in-node-map (node-map)
  (loop for start-node across (find-starting-points node-map)
        collect (find-end-of-path (node-id start-node) node-map)))

(defun follow-multiple-paths-simultaneously (node-map)
  (let ((path-ends (find-path-ends-in-node-map node-map)))
    (reduce #'lcm path-ends)))

(defun part2 ()
  (let ((node-map (parse-file "input8" (parse-node-map))))
    (follow-multiple-paths-simultaneously node-map)))

(neat-lambda:disable-lambda-syntax)
(currying:disable-currying-syntax)
