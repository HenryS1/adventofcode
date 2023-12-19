(defpackage :day19
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

(in-package :day19)

(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defstruct rule slot operator value destination)

(defstruct workflow name rules default)

;; (defun operator-from-char (c)
;;   (case c
;;     (#\< #'<)
;;     (#\> #'>)
;;     (t (error "Unexpected operator ~a" c))))

(defstruct part x m a s)

(defun char-to-accessor (c)
  (case c
    (#\x #'part-x)
    (#\m #'part-m)
    (#\a #'part-a)
    (#\s #'part-s)))

(defun parse-rule ()
  (sequential (slot (one #'alphanumericp))
              (operator (one #l(or (char= %c #\>) (char= %c #\<))))
              (value *non-negative-int*)
              (_ (char1 #\:))
              (destination (many1 #'alphanumericp))
              (make-rule :slot slot :operator operator :value value
                         :destination destination)))

(defun parse-workflow ()
  (sequential (name (many1 #'alphanumericp))
              (_ (char1 #\{))
              (rules (sep-by (parse-rule) (char1 #\,)))
              (_ (char1 #\,))
              (default (many1 #'alphanumericp))
              (_ (char1 #\}))
              (make-workflow :name name :rules rules :default default)))

(defun parse-part ()
  (sequential (_ (char1 #\{))
              (_ (seq "x="))
              (x *non-negative-int*)
              (_ (seq ",m="))
              (m *non-negative-int*)
              (_ (seq ",a="))
              (a *non-negative-int*)
              (_ (seq ",s="))
              (s *non-negative-int*)
              (_ (char1 #\}))
              (make-part :x x :m m :a a :s s)))

(defun parse-workflows ()
  (fmap #l(loop with workflows = (make-hash-table :test 'equal)
                for workflow in %workflows
                do (setf (gethash (workflow-name workflow) workflows) workflow)
                finally (return workflows))
        (sep-by (parse-workflow) (one #'newlinep))))

(defun parse-parts ()
  (sep-by (parse-part) (one #'newlinep)))

(defun parse-instructions ()
  (sequential (workflows (parse-workflows))
              (_ (many1 #'newlinep))
              (parts (parse-parts))
              (cons workflows parts)))

(defun read-instructions-from-file (filename)
  (parse-file filename (parse-instructions)))

;; (defun apply-rule (rule part)
;;   (bind ((part-value (funcall (rule-slot rule) part))
;;          (rule-applies (funcall (rule-operator rule) part-value (rule-value rule))))
;;     (when rule-applies
;;       (rule-destination rule))))

;; (defun find-destination (part workflow)
;;   (loop for rule in (workflow-rules workflow)
;;         for destination = (apply-rule rule part)
;;         when destination do (return destination)
;;           finally (return (workflow-default workflow))))

;; (defun process-part (part workflows)
;;   (labels ((rec (current-workflow)
;;              (let ((destination (find-destination part current-workflow)))
;;                (cond ((equal destination "A") t)
;;                      ((equal destination "R") nil)
;;                      (t (rec (gethash destination workflows)))))))
;;     (rec (gethash "in" workflows))))

(defun rating-sum (part) 
  (+ (part-x part) (part-m part) (part-a part) (part-s part)))

(defstruct rule-combination applied)

(defun not-applied-to-applied (rule)
  (let ((new-operator (case (rule-operator rule)
                        (#\< #\G)
                        (#\> #\L))))
    (make-rule :slot (rule-slot rule)
               :operator new-operator 
               :value (rule-value rule)
               :destination (rule-destination rule))))

(defun find-successful-combinations-of-rules (workflows)
  (labels ((rec (current-workflow applied-so-far not-applied-so-far)
             (let ((default-result 
                     (cond ((equal (workflow-default current-workflow) "A")
                            (list (make-rule-combination
                                   :applied (append applied-so-far
                                                    (mapcar #'not-applied-to-applied
                                                            (append not-applied-so-far
                                                                    (workflow-rules
                                                                     current-workflow)))))))
                           ((equal (workflow-default current-workflow) "R") nil)
                           (t (rec (gethash (workflow-default current-workflow) workflows)
                                   applied-so-far
                                   (append (workflow-rules current-workflow) 
                                           not-applied-so-far))))))
               (loop with successful = nil
                     for rule in (workflow-rules current-workflow)
                     for destination = (rule-destination rule)
;;                     do (format t "RULE ~a~%" rule)
                     if (equal destination "A")
                       do (push (make-rule-combination 
                                 :applied (append (cons rule applied-so-far)
                                                  (mapcar #'not-applied-to-applied
                                                          not-applied-so-far))) successful)
;                          (format t "ACCEPTED ~a~%" successful)
                     else do (let ((rest (when (not (equal destination "R"))
                                           (rec (gethash destination workflows)
                                                (cons rule applied-so-far)
                                                not-applied-so-far))))
;                               (format t "REST ~a ~a~%" rest successful)
                               (setf successful (append rest successful)))
                     finally (return (progn 
;                                       (format t "RESULT ~a~%" (append successful default-result))
                                       (append successful default-result)))))))
    (rec (gethash "in" workflows) nil nil)))

(defstruct available-per-slot 
  (x (cons 1 4000)) 
  (m (cons 1 4000)) 
  (a (cons 1 4000)) 
  (s (cons 1 4000)))

(defun intersect-rule-with-range (rule range)
  (bind ((value (rule-value rule)))
    (when range
      (case (rule-operator rule)
        (#\> (when (< value (cdr range))
               (cons (max (car range) (+ value 1)) (cdr range))))
        (#\< (when (> value (car range))
               (cons (car range) (min (- value 1) (cdr range)))))
        (#\G (cons (max (car range) value) (cdr range)))
        (#\L (cons (car range) (min (cdr range) value)))))))

;; (defun find-available-for-slot (rules-for-slot)
;;   (loop with range = (cons 0 4000)
;;         for rule in rules-for-slot
;;         do (setf range (intersect-rule-with-range ))))

;; (defun find-available-per-slot (rule-combination)
;;   (loop with available = 
;; for rule in (rule-combination-applied rule-combination)
;;         ))

(defun combine-rules (rule-combination)
  (loop with ranges-per-slot = (make-available-per-slot)
        for rule in (rule-combination-applied rule-combination)
        do (case (rule-slot rule)
             (#\x (setf (available-per-slot-x ranges-per-slot)
                        (intersect-rule-with-range rule (available-per-slot-x ranges-per-slot))))
             (#\m (setf (available-per-slot-m ranges-per-slot)
                        (intersect-rule-with-range rule (available-per-slot-m ranges-per-slot))))
             (#\a (setf (available-per-slot-a ranges-per-slot)
                        (intersect-rule-with-range rule (available-per-slot-a ranges-per-slot))))
             (#\s (setf (available-per-slot-s ranges-per-slot)
                        (intersect-rule-with-range rule (available-per-slot-s ranges-per-slot)))))
        finally (return ranges-per-slot)))

(defun range-count (range)
  (if (null range)
      0
      (+ 1 (- (cdr range) (car range)))))

(defun available-count (available-per-slot)
  (let ((for-x (range-count (available-per-slot-x available-per-slot)))
        (for-m (range-count (available-per-slot-m available-per-slot)))
        (for-a (range-count (available-per-slot-a available-per-slot)))
        (for-s (range-count (available-per-slot-s available-per-slot))))
    (* for-x for-m for-a for-s)))

(defun intersect-ranges (one other)
  (cond ((> (car one) (car other)) (intersect-ranges other one))
        ((<= (car one) (car other) (cdr other) (cdr one)) other)
        ((<= (car one) (car other) (cdr one)) (cons (car other) (cdr one)))
        (t nil)))

(defun find-overlap (one other)
  (let ((for-x (intersect-ranges (available-per-slot-x one)
                                 (available-per-slot-x other)))
        (for-m (intersect-ranges (available-per-slot-m one)
                                 (available-per-slot-m other)))
        (for-a (intersect-ranges (available-per-slot-a one)
                                 (available-per-slot-a other)))
        (for-s (intersect-ranges (available-per-slot-s one)
                                 (available-per-slot-s other))))
    (available-count (make-available-per-slot :x for-x :m for-m :a for-a :s for-s))))

(defun total-overlap (available-slots)
  (loop for available in available-slots
        for remaining = (cdr available-slots) then (cdr remaining)
        summing (reduce #'+ (mapcar #p(find-overlap available) remaining))))

(defun part2 ()
  (bind (((workflows . _) (read-instructions-from-file "../tests/test-input19"))
         (available-slots (mapcar #'combine-rules 
                                  (find-successful-combinations-of-rules workflows))))
;    (find-successful-combinations-of-rules workflows)

    
    (-
     (reduce #'+ 
             (mapcar #'available-count
                     (mapcar #'combine-rules (find-successful-combinations-of-rules workflows))))
     (total-overlap available-slots))))

(neat-lambda:disable-lambda-syntax)
(currying:disable-currying-syntax)
