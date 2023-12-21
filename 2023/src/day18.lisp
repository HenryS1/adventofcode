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

(defstruct line start end)

(defun make-line-segment (one other)
  (cond ((< (get-row one) (get-row other))
         (make-line :start one :end other))
        ((< (get-col one) (get-col other))
         (make-line :start one :end other))
        (t (make-line :start other :end one))))

(defun make-line-segments (path)
  (loop for remaining = path then (cdr remaining)
        for first = (car remaining)
        for second = (cadr remaining)
        while second
        collect (make-line-segment first second)))

;; (defun segments-intersect (one other)
;;   (bind ((one-direction (- (line-end one) (line-start one)))
;;          (x1 (get-row (line-start one)))
;;          (x2 (get-row (line-end one)))
;;          (x3 (get-row (line-start other)))
;;          (x4 (get-row (line-end other)))
;;          (y1 (get-col (line-start one)))
;;          (y2 (get-col (line-end one)))
;;          (y3 (get-col (line-start other)))
;;          (y4 (get-col (line-end other)))
;;          (s1-part1 (- (* (- x1 x3) (- y3 y4)) (* (- y1 y3) (- x3 x4))))
;;          (s1-part2 (- (* (- x1 x2) (- y3 y4)) (* (- y1 y2) (- x3 x4))))
;;          (s2-part1 (- (* (- x1 x3) (- y1 y2)) (* (- y1 y3) (- x1 x2))))
;;          (s2-part2 (- (* (- x1 x2) (- y3 y4)) (* (- y1 y2) (- x3 x4)))))
;;     (when (and (/= s1-part2 0) (/= s2-partt2 0))
;;       (and (if (>= s1-part2 0) (< 0 s1-part1 s1-part2))
;;            (< 0 s2-part1 s2-part2)))))

(defun intersect-segments (one other)
  (bind ((one-direction (- (line-end one) (line-start one)))
;;         (other-direction (- (line-end other) (line-start other)))
         (x1 (get-row (line-start one)))
         (x2 (get-row (line-end one)))
         (x3 (get-row (line-start other)))
         (x4 (get-row (line-end other)))
         (y1 (get-col (line-start one)))
         (y2 (get-col (line-end one)))
         (y3 (get-col (line-start other)))
         (y4 (get-col (line-end other)))
         (s-part1 (- (* (- x1 x3) (- y3 y4)) (* (- y1 y3) (- x3 x4))))
         (s-part2 (- (* (- x1 x2) (- y3 y4)) (* (- y1 y2) (- x3 x4)))))
    (when (/= s-part2 0)
;      (format t "HERE~%")
      (bind ((s (/ s-part1 s-part2))
             (intersection (+ (* s one-direction) (line-start one))))
;        (format t "HERE2 ~a ~a~%" s intersection)
        (when (and (< 0 s 1)                  
                   (<= (min (get-col (line-start other))
                           (get-col (line-end other)))
                      (get-col intersection)
                      (max (get-col (line-start other))
                           (get-col (line-end other))))
                   (<= (min (get-row (line-start other))
                           (get-row (line-end other)))
                      (get-row intersection)
                      (max (get-row (line-start other))
                           (get-row (line-end other)))))
          intersection)))))

(defstruct triangle one two three)

(setf *print-circle* t)

(defun make-circular (list)
  (let ((copy (copy-list list)))
    (setf (cdr (last copy)) copy)))

;; (defun dot-product (one other)
;;   (+ (* (get-row one) (get-row other)) (* (get-col one) (get-col other))))

;; (defun angle-between (one other)
;;   (let ((normalised-product (/ (dot-product one-two one-three)
;;                                (* (abs one-two) (abs one-three)))))
;;     ()))

(defun cross-product (one other)
  (- (* (get-row one) (get-col other)) (* (get-row other) (get-col one))))

(defun right-hand-turn (one two three)
  (let* ((one-two (- two one))
         (one-three (- three one)))
    (< (cross-product one-two one-three) 0)
    ;; (format t "one-two ~a one-three ~a~%" one-two one-three)
    ;; (format t "normalised product ~a ~a~%" normalised-product (acos normalised-product))
    ;; (acos normalised-product)
    ;; (< 
    ;;    0)
    ))

(defun find-direction (one other)
  (cond ((> (get-row other) (get-row one)) 'down)
        ((> (get-col other) (get-col one)) 'right)
        ((< (get-row other) (get-row one)) 'up)
        ((< (get-col other) (get-col one)) 'left)))

(defun cell-coordinate (first second third)
  (bind ((first-direction (find-direction first second))
         (second-direction (find-direction second third)))
    ;; (format t "FIRST DIRECTION ~a SECOND DIRECTION ~a~%" 
    ;;         first-direction second-direction)
    (case first-direction
      (right (case second-direction
               (down 'top-right)
               (up 'top-left)))
      (left (case second-direction
              (down 'bottom-right)
              (up 'bottom-left)))
      (down (case second-direction
              (right 'top-right)
              (left 'bottom-right)))
      (up (case second-direction
            (right 'top-left)
            (left 'bottom-left))))))

(defun adjust-coordinate (cell-coordinate coord)
  (case cell-coordinate
    (bottom-left (+ coord #c(1 0)))
    (bottom-right (+ coord #c(1 1)))
    (top-right (+ coord #c(0 1)))
    (top-left coord)))

(defun change-coordinates (path)
  (let* ((updated-path (reverse (cons (cadr path) (reverse path))))
         (with-new-coordinates (reverse (loop for remaining on updated-path
                   for first = (car remaining)
                   for second = (cadr remaining)
                   for third = (caddr remaining)
                   while third
                   for cell-coordinate = (cell-coordinate first second third)
;                   do (format t "COORD ~a CELL-COORDINATE ~a~%" second cell-coordinate)
                   collect (adjust-coordinate cell-coordinate second)))))
;    (format t "UPDATED PATH ~a~%" updated-path)
    (append with-new-coordinates (list (car with-new-coordinates)))))

(defun ear-clipping (vertices edges)
;  (format t "VERTICES ~a~%" vertices)
  (let (triangles)
    (loop for rest on vertices
          for first = (car rest)
          for second = (cadr rest)
          for third = (caddr rest)
          while (not (equalp third first))
          for interior-edge = (make-line-segment first third)
 ;         do (format t "INTERIOR EDGE ~a~%" interior-edge)
          when (and (right-hand-turn first second third)
                    (not (some #p(intersect-segments interior-edge) edges)))
            do ;(format t "FOUND EAR ~a~%" interior-edge)
;               (format t "HAS INTERSECTION ~a~%" (some #p(intersect-segments interior-edge) edges))
;               (format t "EDGES ~a~%" edges)
               (let ((old-edges (list (make-line-segment first second)
                                      (make-line-segment second third))))
                 (setf edges (remove-if #l(find %edge old-edges :test 'equalp) edges))
                 (push interior-edge edges)
                 (push (make-triangle :one first :two second :three third) 
                       triangles)
                 (setf (cdr rest) (cddr rest))))
    (reverse triangles)))

(defun integer-sqrt (n)
  (let ((root (loop for i from (floor (sqrt (- n 10000)))
               until (> (* i i) n)
               finally (return i))))
    (if (/= (* root root) n)
        (error "Not the square root ~a ~a" n root)
        root)))

(defun area-of-triangle (triangle)
  (let* ((a (abs (- (triangle-one triangle) (triangle-two triangle))))
         (b (abs (- (triangle-two triangle) (triangle-three triangle))))
         (c (abs (- (triangle-one triangle) (triangle-three triangle))))
         (s (/ (+ a b c) 2)))
    (integer-sqrt (* s (- s a) (- s b) (- s c)))))

(defun part2 ()
  (bind ((instructions (read-hex-instructions-from-file "input18"))
         (path (reverse (change-coordinates (reverse (find-vertices instructions))))
;(find-vertices instructions);
               ;; 
               ;; (list #c(0 0) #c(0 7) #c(6 7) #c(6 5) #c(7 5) #c(7 7) #c(10 7) 
               ;;       #c(10 1) #c(8 1) #c(8 0) #c(5 0) #c(5 2) #c(3 2) #c(3 0) #c(0 0))
               )
         (line-segments 
          (progn; (format t "path ~a~%" path)
                 (make-line-segments path ;; (append path (list (car path)))
                                     )))
         (triangles (ear-clipping (make-circular (butlast path)) line-segments)))
;    (format t "Adjusted ~a~%" (change-coordinates (reverse (find-vertices instructions))))
    (list (reverse path) triangles (mapcar #'area-of-triangle triangles) (reduce #'+ (mapcar #'area-of-triangle triangles)))))

(neat-lambda:disable-lambda-syntax)
(currying:disable-currying-syntax)
