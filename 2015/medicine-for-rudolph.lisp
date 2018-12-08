(ql:quickload :cl-ppcre)

(defun is-replacement (line)
  (cl-ppcre:scan "\\w+\\s+=>\\s+\\w+" line))

(defun is-formula (line)
  (and (not (is-replacement line))
       (cl-ppcre:scan "\\w+" line)))

(defun read-replacement (line)
  (multiple-value-bind (m rs) (cl-ppcre:scan-to-strings "(\\w+)\\s+=>\\s+(\\w+)" line)
    (declare (ignore m))
    (map 'list #'identity rs)))

(defun read-replacements (lines)
  (let ((replacements (make-hash-table :test 'equal)))
    (loop for line in lines
       when (is-replacement line)
       do (destructuring-bind (key value) (read-replacement line)
            (push value (gethash key replacements))))
    replacements))

(defun collect-lines ()
  (with-open-file (f "medicine-for-rudolph-input.txt")
    (when f
      (loop for line = (read-line f nil nil)
         while line
         collect line))))

(defun read-input (lines)
  (cons (read-replacements lines)
        (remove-if-not #'is-formula lines)))

(defun find-substitutions (molecule substitutions index max-len)
  (let (found)
    (loop for i from (1+ index) to (min (+ index max-len) (length molecule))
       do (let ((lookup (gethash (subseq molecule index i) substitutions)))
            (when lookup
              (let ((new-subs (cons (list (- i index) lookup) found)))
                (setf found new-subs)))))
    found))

(defun max-substitution-len (substitutions)
  (let ((mx 0))
    (loop for k being the hash-keys of substitutions
       when (> (length k) mx)
       do (setf mx (length k)))
    mx))

(defun generate-substitutions (molecule substitutions)
  (let ((max-len (max-substitution-len substitutions))
        (count 0)
        (seen (make-hash-table :test 'equal))
        results)
    (loop for i from 0 to (1- (length molecule))
       do (loop for subs in (find-substitutions molecule substitutions i max-len)
             do (destructuring-bind (len vs) subs
                  (loop for sub in vs
                     do (let ((new-string (concatenate 'string (subseq molecule 0 i)
                                                       sub
                                                       (subseq molecule (+ i len)))))
                          (when (and (not (gethash new-string seen)))
                            (incf count)
                            (push new-string results))
                          (setf (gethash new-string seen) t))))))   
    (values count seen (sort results #'string<))))

(defun generate-substitutions-with-cb (molecule substitutions callback)
  (let ((max-len (max-substitution-len substitutions))
        (count 0)
        (seen (make-hash-table :test 'equal)))
    (loop for i from 0 to (1- (length molecule))
       do (loop for subs in (find-substitutions molecule substitutions i max-len)
             do (destructuring-bind (len vs) subs
                  (loop for sub in vs
                     do (let ((new-string (concatenate 'string (subseq molecule 0 i)
                                                       sub
                                                       (subseq molecule (+ i len)))))
                          (when (not (gethash new-string seen))
                            (funcall callback new-string))
                          (setf (gethash new-string seen) t))))))
    (values count seen)))

(defun generate-molecule (current target moves substitutions)
  (multiple-value-bind (count generated) (generate-substitutions current substitutions)
    (declare (ignore count))
    (cond ((> (length current) (length target)) nil)
          ((and (= (length current) (length target)) (string= current target)) moves)
          (t (let ((mn 9999999999))
               (loop for possibility being the hash-keys of generated
                  do (let ((result (generate-molecule possibility
                                                      target (1+ moves) substitutions)))
                       (when (and result (< result mn))
                         (setf mn result))))
               mn)))))

(defun reverse-substitutions (substitutions)
  (let ((reversed (make-hash-table :test 'equal)))
    (loop for head being the hash-keys of substitutions using (hash-value body)
       do (loop for sub in body
             do (push head (gethash sub reversed))))
    reversed))

(defun generate-reductions (molecule substitutions)
  (generate-substitutions molecule substitutions))

(defun reduce-molecule (current substitutions)
  (let ((seen (make-hash-table :test 'equal))
        (mn 9999999999))
    (labels ((recur (current moves)
               (multiple-value-bind (count subs reductions)
                     (generate-reductions current substitutions)
                   (declare (ignore count subs))
                   (loop for reduction in reductions
                      when (not (gethash reduction seen))
                      do (when (< (length reduction) 30)
                           (format t "~a~%" (length reduction)))
                        (cond
                          ((string= current "e")
                           (progn
                             (format t "FOUND END~%")
                             (when (< moves mn)
                               (setf mn moves))))
                          (t (recur reduction (1+ moves))
                             (when (< (length reduction) 200)
                               (setf (gethash reduction seen) t))))))))
      (recur current 0))
    mn))

(defun solution-part-1 ()
  (destructuring-bind (substitutions molecule) (read-input (collect-lines))
    (generate-substitutions molecule substitutions)))

(defun solution-part-2 ()
  (destructuring-bind (substitutions molecule) (read-input (collect-lines))
    (reduce-molecule molecule (reverse-substitutions substitutions))))
