(defpackage :day3
  (:use 
   :cl 
   :iterate 
   :alexandria 
   :anaphora 
   :pears
   :metabang-bind)
  (:export
   :number-length
   :find-numbers-and-parts
   :read-map-from-file
   :part-map-numbers
   :part-map-parts
   :part-map-coord-to-number
   :make-number-coord
   :is-part-number
   :find-part-numbers
   :find-gear-ratio
   :all-gear-ratios
   :neighbouring-numbers))

(in-package :day3)

(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun read-map-from-file (filename)
  (parse-file filename
              (fmap #l(coerce %lines 'vector)
                    (lines (many #l(not (newlinep %c)))))))

(defun read-map ()
  (read-map-from-file "input3"))

(defun number-length (n)
  (loop for len from 1
        for remaining = (floor n 10) then (floor remaining 10)
        while (> remaining 0)
        finally (return len)))

(defstruct number-coord row col length)
(defstruct part-map numbers parts coord-to-number)

(defun find-numbers-and-parts (grid)
  (loop with numbers = (make-hash-table)
        with coord-to-number = (make-hash-table :test 'equal)
        with parts = (make-hash-table :test 'equal)
        for row-number from 0 to (- (length grid) 1)
        for row = (aref grid row-number)
        do (loop for col-number from 0 to (- (length row) 1)
                 for c = (aref row col-number)
                 for n = (when (digit-char-p c)
                           (parse-integer row :start col-number :junk-allowed t))
                 when n
                   do (let ((n-length (number-length n)))
                        (push (make-number-coord :row row-number 
                                                 :col col-number
                                                 :length n-length)
                              (gethash n numbers))
                        (loop for c from col-number to (+ col-number (- n-length 1))
                              do (setf (gethash (complex row-number c) coord-to-number) n))
                        (incf col-number (- n-length 1)))
                 when (and (not (digit-char-p c)) (char/= c #\.))
                   do (setf (gethash (complex row-number col-number) parts) c))
        finally (return (make-part-map :numbers numbers
                                       :parts parts
                                       :coord-to-number coord-to-number))))

(defun is-part-number (number-coord input-map)
  (loop with columns = (length (aref input-map 0))
        for row-number from (max 0 (- (number-coord-row number-coord) 1)) 
          to (min (- (length input-map) 1) (+ (number-coord-row number-coord) 1))
        for row = (aref input-map row-number)
        for is-part-number = (loop for col-number 
                                   from (max 0 (- (number-coord-col number-coord) 1))
                                     to (min (- columns 1) 
                                             (+ (number-coord-col number-coord) 
                                                (number-coord-length number-coord)))
                                   for c = (aref row col-number)
                                   when (and (not (digit-char-p c)) (char/= c #\.))
                                     do (return t)
                                   finally (return nil))
        when is-part-number
          do (return is-part-number)
        finally (return nil)))

(defun repeat (e n)
  (loop for i from 1 to n collect e))

(defun find-part-numbers (part-map input-map)
  (loop for number being the hash-keys of (part-map-numbers part-map) 
        using (hash-value number-coords)
        for part-adjacent-coords = (remove-if-not #l(is-part-number %number-coord input-map)
                                                  number-coords)
        appending (repeat number (length part-adjacent-coords))))

(defun part1 ()
  (let* ((input-map (read-map-from-file "input3"))
         (part-map (find-numbers-and-parts input-map)))
    (reduce #'+ (find-part-numbers part-map input-map))))

(defun neighbouring-numbers (coord part-map input-map)
  (loop with neighbours-set = (make-hash-table)
        with rows = (length input-map)
        with cols = (length (aref input-map 0))
        for row-number from (max (- (realpart coord) 1) 0) 
          to (min (+ (realpart coord) 1) (- rows 1))
        do (loop for col-number from (max (- (imagpart coord) 1) 0)
                   to (min (+ (imagpart coord) 1) (- cols 1))
                 for n = (gethash (complex row-number col-number)
                                  (part-map-coord-to-number part-map))
                 when n do (setf (gethash n neighbours-set) t))
        finally (return (alexandria:hash-table-keys neighbours-set))))

(defun find-gear-ratio (coord part-map input-map)
  (let ((numbers (neighbouring-numbers coord part-map input-map)))
    (when (= (length numbers) 2)
      (reduce #'* numbers))))

(defun all-gear-ratios (part-map input-map)
  (loop for coord being the hash-keys of (part-map-parts part-map)
        using (hash-value part)
        for gear-ratio = (find-gear-ratio coord part-map input-map)
        when (and (char= part #\*) gear-ratio) collect gear-ratio))

(defun part2 ()
  (let* ((input-map (read-map-from-file "input3"))
         (part-map (find-numbers-and-parts input-map)))
    (reduce #'+ (all-gear-ratios part-map input-map))))

(neat-lambda:disable-lambda-syntax)
(currying:disable-currying-syntax)
