(defpackage :day13
  (:use 
   :cl 
   :iterate 
   :alexandria 
   :anaphora 
   :pears
   :metabang-bind
   :queue)
  (:export
   :parse-pattern
   :find-horizontal-reflection
   :find-vertical-reflection
   :find-value-of-patterns
   :parse-patterns))

(in-package :day13)

(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun parse-pattern ()
  (fmap #l(coerce %lines 'vector)
        (sep-by (many1 #l(not (newlinep %c))) (one #'newlinep))))

(defun parse-patterns ()
  (sep-by (parse-pattern) (many1 #'newlinep)))

(defun read-patterns-from-file (filename)
  (parse-file filename (parse-patterns)))

(defun reflects-after-row (pattern row smudge)
  (loop with mismatch-count = 0
        for r-num-above from row downto 0
        for above-row = (aref pattern r-num-above)
        for r-num-below from (+ row 1) to (- (length pattern) 1)
        for below-row = (aref pattern r-num-below)
        for mirrored = (loop for c-above across above-row
                             for c-below across below-row
                             when (char/= c-above c-below)
                               do (incf mismatch-count)
                             when (> mismatch-count smudge)
                               do (return nil)
                             finally (return t))
        when (not mirrored)
          do (return nil)
        finally (return (= mismatch-count smudge))))

(defun find-horizontal-reflection (pattern &key (smudge 0))
  (loop for row-num from 0 to (- (length pattern) 2)
        when (reflects-after-row pattern row-num smudge)
          do (return row-num)
        finally (return nil)))

(defun reflects-after-column (pattern col smudge)
  (loop with mismatch-count = 0
        for c-num-before from col downto 0
        for c-num-after from (+ col 1) to (- (length (aref pattern 0)) 1)
        for mirrored = (loop for row-num from 0 to (- (length pattern) 1)
                             for c-before = (aref (aref pattern row-num) c-num-before)
                             for c-after = (aref (aref pattern row-num) c-num-after)
                             when (char/= c-before c-after)
                               do (incf mismatch-count)
                             when (> mismatch-count smudge)
                               do (return nil)
                             finally (return t))
        when (not mirrored)
          do (return nil)
        finally (return (= mismatch-count smudge))))

(defun find-vertical-reflection (pattern &key (smudge 0))
  (loop for col-num from 0 to (- (length (aref pattern 0)) 2)
        when (reflects-after-column pattern col-num smudge)
          do (return col-num)
        finally (return nil)))

(defun find-value-of-patterns (patterns &key (smudge 0))
  (loop with rows-above = 0
        with cols-to-left = 0
        for pattern in patterns
        do (acond 
             ((find-horizontal-reflection pattern :smudge smudge) (incf rows-above (+ it 1)))
             ((find-vertical-reflection pattern :smudge smudge) (incf cols-to-left (+ it 1)))
             (t (error "No reflection")))
        finally (return (+ cols-to-left (* 100 rows-above)))))

(defun part1 ()
  (let ((patterns (read-patterns-from-file "input13")))
    (find-value-of-patterns patterns)))

(defun part2 ()
  (let ((patterns (read-patterns-from-file "input13")))
    (find-value-of-patterns patterns :smudge 1)))

(neat-lambda:disable-lambda-syntax)
(currying:disable-currying-syntax)
