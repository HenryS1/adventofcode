(load "utilities.lisp")
(ql:quickload :cl-ppcre)

(defparameter *input-file* "experimental-emergency-teleportation-input.txt")

(defun read-nano-bot (line)
  (multiple-value-bind (m rs)
      (cl-ppcre:scan-to-strings ".*<(-?\\d+),(-?\\d+),(-?\\d+)>,\\s+r=(\\d+)" line)
    (declare (ignore m))
    (map 'list #'parse-integer rs)))

(defun read-input (filename)
  (read-lines filename #'read-nano-bot))

(defun signal-strength (bot)
  (cadddr bot))

(defun find-strongest-bot (bots)
  (loop with best = nil
     with strength = 0
     for bot in bots
     when (> (cadddr bot) strength)
     do (setf best bot
              strength (signal-strength bot))
     finally (return best)))

(defun manhattan-distance (one other)
  (+ (abs (- (car one) (car other)))
     (abs (- (cadr one) (cadr other)))
     (abs (- (caddr one) (caddr other)))))

(defun in-range (one)
  (lambda (other) 
    (<= (manhattan-distance one other) (signal-strength one))))

(defun solution-part-1 ()
  (let* ((bots (read-input *input-file*))
         (strongest (find-strongest-bot bots)))
    (length (remove-if-not (in-range strongest) bots))))


