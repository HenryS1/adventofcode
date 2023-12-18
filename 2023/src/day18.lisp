(defpackage :day18
  (:use 
   :cl 
   :iterate 
   :anaphora 
   :alexandria
   :pears
   :metabang-bind
   :priority-queue
   :queue)
  (:export
   ))

(in-package :day18)

(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defstruct instruction direction count red green blue)

(defun parse-color ()
  (fmap #l(parse-integer %color :radix 16) (manyn #'alphanumericp 2)))

(defun parse-instruction ()
  (sequential (direction (one #'alpha-char-p))
              (_ (many1 #p(char= #\space)))
              (count *non-negative-int*)
              (_ (many1 #p(char= #\space)))
              (_ (seq "(#"))
              (red (parse-color))
              (green (parse-color))
              (blue (parse-color))
              (_ (one #p(char= #\))))
              (make-instruction :direction direction :count count 
                                :red red :green green :blue blue)))

(defun parse-instructions ()
  (sep-by (parse-instruction) (many1 #'newlinep)))

(defun read-instructions-from-file (filename)
  (parse-file filename (parse-instructions)))

(defun make-coord (row col)
  (complex row col))

(defun get-row (coord)
  (realpart coord))

(defun get-col (coord)
  (imagpart coord))

(defun next-offset (direction)
  (case direction
    (#\L (-  #c(0 1)))
    (#\R #c(0 1))
    (#\U (- #c(1 0)))
    (#\D #c(1 0))))

(defun dig-trench (instructions)
  (let* ((coord (make-coord 0 0))
         (boundary (list coord)))
    (loop for instruction in instructions 
          for direction = (instruction-direction instruction)
          for count = (instruction-count instruction)
          for offset = (next-offset direction)
          do (loop for i from 1 to count
                   do (incf coord offset)
                      (push coord boundary)))
    boundary))

(defstruct bounds max-row min-row max-col min-col)

(defun make-set (boundary)
  (loop with set = (make-hash-table)
        for coord in boundary 
        do (setf (gethash coord set) t)
        finally (return set))) 

(defun find-bounds (boundary)
  (loop for coord in boundary
        maximizing (get-row coord) into max-row
        minimizing (get-row coord) into min-row
        maximizing (get-col coord) into max-col
        minimizing (get-col coord) into min-col
        finally (return (make-bounds :max-row max-row 
                                     :min-row min-row
                                     :max-col max-col 
                                     :min-col min-col))))

(defun neighbours (coord boundary seen)
  (let (neighbours
        (down (+ coord #c(1 0)))
        (up (- coord #c(1 0)))
        (right (+ coord #c(0 1)))
        (left (- coord #c(0 1))))
;    (format t "UP ~a DOWN ~a RIGHT ~a LEFT ~a~%" up down right left)
    (when (and (not (gethash down boundary))
               (not (gethash down seen)))
      (push down neighbours))
    (when (and (not (gethash up boundary))
               (not (gethash up seen)))
      (push up neighbours))
    (when (and (not (gethash right boundary))
               (not (gethash right seen)))
      (push right neighbours))
    (when (and (not (gethash left boundary))
               (not (gethash left seen)))
      (push left neighbours))
    neighbours))

(defun fill-connected (boundary boundary-set start seen)
;  (format t "FILL CONNECTED START ~a~%" start)
  (when (not (gethash start seen))
    (setf (gethash start seen) t))
  (loop with bounds = (find-bounds boundary)
        with stack = (list start)
        for coord = (pop stack)
        while coord
        for neighbours = (neighbours coord boundary-set seen)
;        do (format t "COORD ~a~%" coord)
        when (or (> (get-row coord) (bounds-max-row bounds))
                 (< (get-row coord) (bounds-min-row bounds))
                 (> (get-col coord) (bounds-max-col bounds))
                 (< (get-col coord) (bounds-min-col bounds)))
          do (return nil)
;        do (format t "NEIGHBOURS ~a~%" neighbours)
        do (loop for neighbour in neighbours
                 ;; do (format t "GET NEIGHBOUR ~a ~a~%" 
                 ;;            (gethash neighbour boundary-set)
                 ;;            (gethash neighbour seen))
                 when (and (not (gethash neighbour boundary-set))
                           (not (gethash neighbour seen)))
                   do (push neighbour stack)
;                      (format t "PUSHING NEIGHBOUR ~a~%" neighbour)
                      (setf (gethash neighbour seen) t))
        finally (return seen)))

(defun extend-boundary (boundary)
  (cons (cadr boundary) (reverse boundary)))

(defun one-side (diff negate)
  (let ((offset (case diff 
                  (#c(0 1) #c(1 0))
                  (#c(0 -1) #c(-1 0))
                  (#c(1 0) #c(0 -1))
                  (#c(-1 0) #c(0 1)))))
    (if negate (- offset) offset)))

(defun fill-interior (boundary boundary-set right-hand)
  (loop with interior = (make-hash-table)
        for remaining = (extend-boundary boundary) then (cdr remaining)
        for first = (car remaining)
        for second = (cadr remaining)
        for third = (caddr remaining)
        while third
        for first-diff = (- second first)
        for second-diff = (- third second)
        for first-side = (+ (one-side first-diff right-hand) second)
        for second-side = (+ (one-side second-diff right-hand) third)
        for inside = (and (or (gethash first-side boundary-set)
                              (fill-connected boundary boundary-set first-side interior))
                          (or (gethash second-side boundary-set) 
                              (fill-connected boundary boundary-set second-side interior)))
;        do (format t "FIRST-SIDE ~a SECOND-SIDE ~a~%" first-side second-side)
        when (not inside)
          do (return nil)
        finally (return (cons ;; (hash-table-alist interior) 
                              ;; (hash-table-alist boundary-set)
                              interior boundary-set
                         ))))

(defun direction-from-number (n)
  (case n
    (0 #\R)
    (1 #\D)
    (2 #\L)
    (3 #\U)))

(defun parse-instruction-from-hex ()
  (sequential (_ (one #'alpha-char-p))
              (_ (many1 #p(char= #\space)))
              (_ *non-negative-int*)
              (_ (many1 #p(char= #\space)))
              (_ (seq "(#"))
              (count (fmap #l(parse-integer %hex :radix 16) (manyn #'alphanumericp 5)))
              (direction (fmap #l(direction-from-number (parse-integer %hex :radix 16)) 
                               (manyn #'alphanumericp 1)))
              (_ (one #p(char= #\))))
              (make-instruction :direction direction :count count)))

(defun parse-instructions-from-hex ()
  (sep-by (parse-instruction-from-hex) (many1 #'newlinep)))

(defun read-hex-instructions-from-file (filename)
  (parse-file filename (parse-instructions-from-hex)))

(defun part1 ()
  (bind ((instructions (read-instructions-from-file "input18"))
         (boundary (dig-trench instructions))
         (boundary-set (make-set boundary)))
    (fill-interior boundary boundary-set nil)))

(defun find-vertices (instructions)
  (loop with coord = (make-coord 0 0)
        with path = (list coord)
        for instruction in instructions
        for direction = (instruction-direction instruction)
        for count = (instruction-count instruction)
        for offset = (next-offset direction)
        do (setf coord (+ (* count offset) coord))
           (push coord path)
        finally (return path)))

(defun is-vertical (line-segment)
  (= (get-col (car line-segment)) (get-col (cdr line-segment))))

(defun make-line-segment (one other)
  (cond ((< (get-row one) (get-row other))
         (cons one other))
        ((< (get-col one) (get-col other))
         (cons one other))
        (t (cons other one))))

(defun make-line-segments (path)
  (loop for remaining = path then (cdr remaining)
        for first = (car remaining)
        for second = (cadr remaining)
        while second
        for segment = (make-line-segment first second)
        if (is-vertical segment)
          collect segment into verticals
        else collect segment into horizontals
        finally (return (cons horizontals verticals))))

(defun intersect-lines (horizontal vertical)
  ;; (cond ((< (get-row (car vertical)) (get-row (car horizontal)) (get-row (cdr vertical)))
  ;;        (make-coord (get-row (car horizontal)) (get-col (car vertical))))
  ;;       ((< (get-col (car horizontal)) (get-col (car vertical)) (get-col (cdr horizontal)))
  ;;        (make-coord (get-row (car horizontal)) (get-col (car vertical))))
  ;;       (t nil))
  (make-coord (get-row (car horizontal)) (get-col (car vertical))))

(defun intersect-all-lines (horizontals verticals)
;  (format t "HORIZONTALS ~a VERTICALS ~a~%" horizontals verticals)
  (loop with intersections = nil
        for horizontal in horizontals
        do (loop for vertical in verticals
                 for intersection = (intersect-lines horizontal vertical)
;                 do (format t "HORIZONTAL ~a VERTICAL ~a~%" horizontal vertical)
                 when intersection
                   do (push intersection intersections)
;                      (format t "FOUND INTERSECTION~%")
                 )
        finally (return (mapcar #'car (hash-table-alist (make-set intersections))))))

(defun coord-less (one other)
  (or (< (get-row one) (get-row other))
      (and (= (get-row one) (get-row other))
           (< (get-col one) (get-col other)))))

(defun sort-coords (coords)
  (sort coords #'coord-less))

(defun find-all-sorted-vertices (instructions)
  (bind ((path (find-vertices instructions))
         ((horizontals . verticals) (make-line-segments path))
         (intersections (intersect-all-lines horizontals verticals)))
    (cons (sort-coords (mapcar #'car
                               (hash-table-alist
                                (make-set 
                                 (append (cdr path) intersections))))) path)))


(defun part2 ()
  (bind ((instructions (read-hex-instructions-from-file "../tests/test-input18"))
         (boundary (dig-trench instructions))
         (boundary-set (make-set boundary)))
    (fill-interior boundary boundary-set nil)))

(defun group-by-row (intersections)
  (loop with grouping = (make-hash-table)
        for intersection in intersections
        do (push intersection (gethash (get-row intersection) grouping))
        finally (return (loop with ordered = (make-hash-table)
                              for row being the hash-keys of grouping 
                                using (hash-value group)
                              do (setf (gethash row ordered) 
                                       (sort group #'< :key #'get-col))
                              finally (return ordered)))))

(defun group-by-col (intersections)
  (loop with grouping = (make-hash-table)
        for intersection in intersections
        do (push intersection (gethash (get-col intersection) grouping))
        finally (return (loop with ordered = (make-hash-table)
                              for col being the hash-keys of grouping 
                              using (hash-value group)
                              do (setf (gethash col ordered)
                                       (sort group #'< :key #'get-row))
                              finally (return ordered)))))

(defun next-in-group (coord grouping)
  (loop for remaining = grouping then (cdr remaining)
        until (= coord (car remaining))
        finally (return (cadr remaining))))

(defstruct rectangle top-left top-right bottom-right bottom-left marking)

(defun rectangle-from-top-left (vertex row-grouping col-grouping)
  (let* ((top-right (next-in-group vertex (gethash (get-row vertex) row-grouping)))
         (bottom-left (next-in-group vertex (gethash (get-col vertex) col-grouping))))
    (when (and top-right bottom-left)
      (let ((bottom-right (next-in-group top-right (gethash (get-col top-right) col-grouping))))
        (when bottom-right
          (make-rectangle :top-left vertex 
                    :top-right top-right 
                    :bottom-right bottom-right
                    :bottom-left bottom-left))))))

(defun rectangle-edges (rectangle)
  (list (cons (rectangle-top-left rectangle) (rectangle-top-right rectangle))
        (cons (rectangle-top-right rectangle) (rectangle-bottom-right rectangle))
        (cons (rectangle-top-left rectangle) (rectangle-bottom-left rectangle))
        (cons (rectangle-bottom-left rectangle) (rectangle-bottom-right rectangle))))

(defun make-rectangles (intersections)
  (bind ((grouped-by-row (group-by-row intersections))
         (grouped-by-col (group-by-col intersections)))
    (loop with grouped-by-edge = (make-hash-table :test 'equal)
          for intersection in intersections
          for rectangle = (rectangle-from-top-left intersection grouped-by-row grouped-by-col)
          when rectangle
          do (loop for edge in (rectangle-edges rectangle)
                   do (push rectangle (gethash edge grouped-by-edge)))
          when rectangle collect rectangle into rectangles
          finally (return (cons grouped-by-edge rectangles)))))

(defun make-edge (one other)
  (if (coord-less one other)
      (cons one other)
      (cons other one)))

(defun is-right (rectangle start end)
  (or (and (= start (rectangle-top-left rectangle)) (= end (rectangle-top-right rectangle)))
      (and (= start (rectangle-top-right rectangle)) (= end (rectangle-bottom-right rectangle)))
      (and (= start (rectangle-bottom-right rectangle)) (= end (rectangle-bottom-left rectangle)))
      (and (= start (rectangle-bottom-left rectangle)) (= end (rectangle-top-left rectangle)))))

(defun mark-rectangles (rectangles-by-edge path)
  (loop for remaining = path then (cdr remaining)
        for first = (car remaining)
        for second = (cadr remaining)
        while second
        for edge = (make-edge first second)
        for rectangles = (gethash edge rectangles-by-edge)
;        do (format t "EDGE ~a RECTANGLES ~a~%" edge rectangles)
        do (loop for rectangle in rectangles
                 if (is-right rectangle first second)
                   do (setf (rectangle-marking rectangle) 'right)
                 else do (setf (rectangle-marking rectangle) 'left))))

(defun mark-neighbours (rectangles-by-edge rectangle)
;  (format t "MARK NEIGHBOURS ~a~%" rectangle)
  (loop for edge in (rectangle-edges rectangle)
        do (loop for other in (gethash edge rectangles-by-edge)
;;                 do (format t "NEIGHBOUR ~a~%" other)
                 when (not (rectangle-marking other))
                   do (setf (rectangle-marking other) 'right)
                      (mark-neighbours rectangles-by-edge other))))

(defun flood-fill (rectangles-by-edge rectangles)
  (loop for rectangle in rectangles
        when (equal (rectangle-marking rectangle) 'right)
          do (mark-neighbours rectangles-by-edge rectangle)))

(defun find-right-rectangles (instructions)
  (bind (((vertices . path) (find-all-sorted-vertices instructions))
         ((rectangles-by-edge . rectangles)
          (make-rectangles vertices)))
    (mark-rectangles rectangles-by-edge (reverse path))
    (flood-fill rectangles-by-edge rectangles)
    (let ((right-rectangles (remove-if-not #l(equal (rectangle-marking %rectangle) 'right)
                                           rectangles)))
      right-rectangles)))

(defun rectangle-perimeter (rectangle)
  (- (+ (* 2 (+ 1 (get-col (- (rectangle-top-right rectangle) (rectangle-top-left rectangle)))))
        (* 2 (+ 1 (get-row (- (rectangle-bottom-left rectangle) (rectangle-top-left rectangle))))))
     4))

(defun rectangle-area-excluding-edges (rectangle)
  (let ((width (+ 1 (get-col
                     (- (rectangle-top-right rectangle)
                        (rectangle-top-left rectangle)))))
        (height (+ 1 (get-row 
                      (- (rectangle-bottom-left rectangle)
                         (rectangle-top-left rectangle))))))
    (- (* width height)
       (rectangle-perimeter rectangle))))

(defun make-edge-set (rectangles)
  (loop with set = (make-hash-table :test 'equal)
        for rectangle in rectangles
        do (loop for edge in (rectangle-edges rectangle)
                 do (setf (gethash edge set) t))
        finally (return set)))

(defun edge-length (edge)
  (let ((diff (- (cdr edge) (car edge))))
    (+ 1 (max (get-col diff) (get-row diff)))))

(defun total-area-of-rectangles (rectangles)
  (let* ((area-sum (reduce #'+ (mapcar #'rectangle-area-excluding-edges rectangles)))
         (edge-set (make-edge-set rectangles))
         (edges-length (loop for edge being the hash-keys of edge-set 
                             summing (edge-length edge))))
    (+ edges-length area-sum)))

(defun count-repeated-points (edge-set)
  (let* ((edges (hash-table-keys edge-set))
         (points (append (mapcar #'car edges) 
                         (mapcar #'cdr edges)))
         (counts (make-hash-table)))
    (loop for point in points
          do (setf (gethash point counts)
                   (+ (gethash point counts 0) 1)))
    (loop for count being the hash-values of counts
          summing (- count 1))))

(defun blah (instructions)
  (total-area-of-rectangles (find-right-rectangles instructions)))

;; (defun mark-rectangles (path grouped-by-vertex)
;;   (loop for remaining = path then (cdr remaining)
;;         for first = (car remaining)
;;         for second = (cdr remaining)
;;         while second
;;         for ))

;; (defun find-rectangles (sorted-vertices)
;;   (let ((bounds (find-bounds sorted-vertices)))
;;     (loop for row from (bounds-min-row bounds)
;;             to (bounds-max-row bounds)
;;           )))

(neat-lambda:disable-lambda-syntax)
(currying:disable-currying-syntax)
