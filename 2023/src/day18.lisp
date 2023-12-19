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

(defun is-vertical (line-segment)
  (= (get-col (line-start line-segment)) (get-col (line-end line-segment))))

(defun is-horizontal (line)
  (= (get-row (line-start line)) (get-row (line-end line))))

;; (defun intersect-vertical-horizontal (vertical other)
;;   ())

;; (defun intersect-lines (one other)
;;   ;; (cond ((< (get-row (car vertical)) (get-row (car horizontal)) (get-row (cdr vertical)))
;;   ;;        (make-coord (get-row (car horizontal)) (get-col (car vertical))))
;;   ;;       ((< (get-col (car horizontal)) (get-col (car vertical)) (get-col (cdr horizontal)))
;;   ;;        (make-coord (get-row (car horizontal)) (get-col (car vertical))))
;;   ;;       (t nil))
;;   (cond (((and (is-vertical one) (is-vertical other)) nil)
;;          ((and (is-vertical one) (is-horizontal other)) (intersect-vertical-horizontal one other))
;;          ((is-vertical one) (intersect-vertical one other))
;;          ((is-horizontal one) (intersect-horizontal one other))
;;          ((is-vertical other) (intersect-lines other one))
;;          ((is-horizontal other) (intersect-lines other one))
;;          (t (intersect-oblique one other))))
;;   (make-coord (get-row (car horizontal)) (get-col (car vertical))))

(neat-lambda:disable-lambda-syntax)
(currying:disable-currying-syntax)
