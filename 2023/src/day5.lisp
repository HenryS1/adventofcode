(defpackage :day5
  (:use 
   :cl 
   :iterate 
   :bind
   :alexandria 
   :anaphora 
   :pears
   :metabang-bind)
  (:export 
   :find-mapped-number
   :find-location-for-seed
   :parse-range-map
   :parse-map
   :parse-almanac-from-file
   :make-range
   :map-range
   :map-ranges-to-locations
   :intersect-range-and-range-mapping
   :mapping-mappings
   :seed-numbers-to-ranges
   :seeds-numbers
   :almanac-seeds))

(in-package :day5)

(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun parse-numbers ()
  (sep-by *non-negative-int* (many1 #p(char= #\space))))

(defstruct seeds numbers)

(defun parse-seeds ()
  (sequential (_ (seq "seeds: "))
              (numbers (parse-numbers))
              (make-seeds :numbers numbers)))

(defstruct range-mapping source-start destination-start length)

(defun parse-range-map ()
  (sequential (destination-start *non-negative-int*)
              (_ (char1 #\space))
              (source-start *non-negative-int*)
              (_ (char1 #\space))
              (range-length *non-negative-int*)
              (make-range-mapping :destination-start destination-start 
                                  :source-start source-start
                                  :length range-length)))

(defstruct mapping name mappings)

(defun parse-map ()
  (sequential (name (many1 #l(not (newlinep %c))))
              (_ (many1 #'newlinep))
              (mappings (sep-by (parse-range-map) (one #'newlinep)))
              (make-mapping :name name :mappings mappings)))

(defstruct almanac seeds maps)

(defun parse-almanac ()
  (sequential (seeds (parse-seeds))
              (_ (many1 #'newlinep))
              (maps (sep-by (parse-map) (many1 #'newlinep)))
              (make-almanac :seeds seeds :maps maps)))

(defun parse-almanac-from-file (filename)
  (parse-file filename (parse-almanac)))

(defun find-mapped-number (number mapping)
  (loop for range in (mapping-mappings mapping)
        when (<= (range-mapping-source-start range) 
                 number
                 (+ (range-mapping-source-start range)
                    (- (range-mapping-length range) 1)))
          do (return (+ (- number (range-mapping-source-start range))
                        (range-mapping-destination-start range)))
        finally (return number)))

(defun find-location-for-seed (seed almanac)
  (loop for mappings in (almanac-maps almanac)
        for number = (find-mapped-number seed mappings)
          then (find-mapped-number number mappings)
        finally (return number)))

(defun part1 ()
  (let* ((almanac (parse-almanac-from-file "input5"))
         (locations (mapcar #l(find-location-for-seed %seed almanac) 
                            (seeds-numbers (almanac-seeds almanac)))))
    (reduce #'min locations)))

(defstruct range start end)

(defun seed-numbers-to-ranges (seed-numbers)
  (loop for remaining = seed-numbers then (cdr (cdr remaining))
        while remaining
        collect (make-range :start (car remaining) :end (+ (car remaining) (cadr remaining)))))

(defun intersect-range-and-range-mapping (range range-mapping)
  (let ((overlap-start (max (range-start range) (range-mapping-source-start range-mapping)))
        (overlap-end (min (range-end range) (+ (range-mapping-source-start range-mapping) 
                                               (- (range-mapping-length range-mapping) 1))))
        mapped-range
        unmapped-ranges)
    (if (<= overlap-start overlap-end)
        (let ((mapped-start (+ (range-mapping-destination-start range-mapping)
                               (- overlap-start (range-mapping-source-start range-mapping))))
              (mapped-end (+ (range-mapping-destination-start range-mapping)
                             (- overlap-end (range-mapping-source-start range-mapping)))))
          (setf mapped-range (make-range :start mapped-start :end mapped-end))        
          (when (< (range-start range) overlap-start)
            (push (make-range :start (range-start range) :end (- overlap-start 1))
                  unmapped-ranges))
          (when (< overlap-end (range-end range))
            (push (make-range :start (+ overlap-end 1) :end (range-end range)) 
                  unmapped-ranges)))
        (push range unmapped-ranges))
    (cons mapped-range unmapped-ranges)))

(defun map-range (range range-mappings)
  (if (null range-mappings)
      (list range)
      (bind (((mapped-range . unmapped-ranges) (intersect-range-and-range-mapping
                                              range (car range-mappings)))
             (rest (apply #'append (mapcar #l(map-range %range (cdr range-mappings)) 
                                           unmapped-ranges))))
      (if mapped-range 
          (cons mapped-range rest)
          rest))))

(defun map-ranges-to-locations (ranges almanac)
  (loop for mapping in (almanac-maps almanac)
        for current-ranges = (alexandria:mappend 
                              #l(map-range %range (mapping-mappings mapping)) ranges)
          then (alexandria:mappend #l(map-range %range (mapping-mappings mapping)) current-ranges)
        finally (return current-ranges)))

(defun part2 ()
  (let* ((almanac (parse-almanac-from-file "input5"))
         (initial-ranges (seed-numbers-to-ranges (seeds-numbers (almanac-seeds almanac))))
         (mapped-ranges (map-ranges-to-locations initial-ranges almanac)))
    (reduce #'min (mapcar #'range-start mapped-ranges))))

(neat-lambda:disable-lambda-syntax)
(currying:disable-currying-syntax)
