(defpackage :day15
  (:use 
   :cl 
   :iterate 
   :anaphora 
   :alexandria
   :pears
   :metabang-bind
   :queue)
  (:export
   :hash-step
   :focusing-power-for-instructions
   :parse-instructions))

(in-package :day15)

(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun parse-steps ()
  (sep-by (many1 #p(char/= #\,)) (one #p(char= #\,))))

(defun read-steps-from-file (filename)
  (parse-file filename (parse-steps)))

(defun hash-character (value c)
  (mod (* (+ value (char-code c)) 17) 256))

(defun hash-step (step)
  (loop for c across step 
        for value = (hash-character 0 c)
          then (hash-character value c)
        finally (return value)))

(defun part1 ()
  (let ((steps (read-steps-from-file "input15")))
    (reduce #'+ (mapcar #'hash-step steps))))

(defstruct instruction label box remove-lens focal-length)

(defun parse-instruction ()
  (orp (sequential (label (many1 #'alpha-char-p))
                   (_ (one #p(char= #\-)))
                   (make-instruction :label label :box (hash-step label) :remove-lens t))
       (sequential (label (many1 #'alpha-char-p))
                   (_ (one #p(char= #\=)))
                   (focal-length *non-negative-int*)
                   (make-instruction :label label :box (hash-step label)
                                     :remove-lens nil :focal-length focal-length))))

(defun parse-instructions ()
  (sep-by (parse-instruction) (one #p(char= #\,))))

(defun read-instructions-from-file (filename)
  (parse-file filename (parse-instructions)))

(defun apply-instruction (instruction boxes)
  (let* ((box-number (instruction-box instruction))
         (lenses (gethash box-number boxes)))
    (if (instruction-remove-lens instruction)
        (setf (gethash box-number boxes)
              (remove-if #l(string= (instruction-label %lens) (instruction-label instruction))
                         lenses))
        (let ((new-lenses (if (find-if #l(string= (instruction-label %lens)
                                                  (instruction-label instruction))
                                       lenses)
                              (mapcar #l(if (string= (instruction-label %lens) 
                                                     (instruction-label instruction))
                                            instruction
                                            %lens) lenses)
                              (cons instruction lenses))))
          (setf (gethash box-number boxes) new-lenses)))))

(defun apply-instructions (instructions)
  (loop with boxes = (make-hash-table)
        for instruction in instructions
        do (apply-instruction instruction boxes)
        finally (return boxes)))

(defun total-focusing-power (boxes)
  (loop with total = 0
        for box-number being the hash-keys of boxes using (hash-value lenses)
        do (loop for lens in (reverse lenses)
                 for i from 1
                 do (incf total (* (+ box-number 1) i (instruction-focal-length lens))))
        finally (return total)))

(defun focusing-power-for-instructions (instructions)
  (total-focusing-power (apply-instructions instructions)))

(defun part2 ()
  (let* ((instructions (read-instructions-from-file "input15")))
    (focusing-power-for-instructions instructions)))

(neat-lambda:disable-lambda-syntax)
(currying:disable-currying-syntax)
