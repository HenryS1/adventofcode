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

(defun operator-from-char (c)
  (case c
    (#\< #'<)
    (#\> #'>)
    (t (error "Unexpected operator ~a" c))))

(defstruct part x m a s)

(defun char-to-accessor (c)
  (case c
    (#\x #'part-x)
    (#\m #'part-m)
    (#\a #'part-a)
    (#\s #'part-s)))

(defun parse-rule ()
  (sequential (slot (fmap #'char-to-accessor (one #'alphanumericp)))
              (operator (one #l(or (char= %c #\>) (char= %c #\<))))
              (value *non-negative-int*)
              (_ (char1 #\:))
              (destination (many1 #'alphanumericp))
              (make-rule :slot slot :operator (operator-from-char operator) :value value
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

(defun apply-rule (rule part)
  (bind ((part-value (funcall (rule-slot rule) part))
         (rule-applies (funcall (rule-operator rule) part-value (rule-value rule))))
    (when rule-applies
      (rule-destination rule))))

(defun find-destination (part workflow)
  (loop for rule in (workflow-rules workflow)
        for destination = (apply-rule rule part)
        when destination do (return destination)
          finally (return (workflow-default workflow))))

(defun process-part (part workflows)
  (labels ((rec (current-workflow)
             (let ((destination (find-destination part current-workflow)))
               (cond ((equal destination "A") t)
                     ((equal destination "R") nil)
                     (t (rec (gethash destination workflows)))))))
    (rec (gethash "in" workflows))))

(defun rating-sum (part) 
  (+ (part-x part) (part-m part) (part-a part) (part-s part)))

(defun part1 ()
  (bind (((workflows . parts) (read-instructions-from-file "input19")))
    (loop for part in parts 
          for accepted = (process-part part workflows)
          if accepted sum (rating-sum part))))

(neat-lambda:disable-lambda-syntax)
(currying:disable-currying-syntax)
