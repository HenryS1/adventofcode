(ql:quickload :cl-ppcre)

(defun is-ready (bot)
  (= (length bot) 2))

(defun parse-goes-to (command)
  (multiple-value-bind  (match groups)
      (cl-ppcre:scan-to-strings "[a-zA-Z\\s]+(\\d+)[a-zA-Z\\s]+(\\d+)\\s*" command)
    (declare (ignore match))
    (eval `(lambda (bots outputs)
        (declare (ignore outputs))
        (progn (push ,(parse-integer (aref groups 0)) 
                     (gethash ,(parse-integer (aref groups 1)) bots))
               t)))))

(defun parse-gives (command)
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings
       "bot\\s+(\\d+).*(output|bot)\\s+(\\d+)[a-zA-Z\\s]+(bot|output)\\s+(\\d+)\\s*" command)
    (declare (ignore match))
    (let ((lookup-destination-low `(gethash ,(parse-integer (aref groups 2))
                                            ,(if (string= (aref groups 1) "output")
                                                 'outputs
                                                 'bots)))
          (lookup-destination-high `(gethash ,(parse-integer (aref groups 4))
                                             ,(if (string= (aref groups 3) "output")
                                                  'outputs
                                                  'bots))))
      (eval `(lambda (bots outputs) 
          (declare (ignorable bots outputs))
          (let ((source-bot (gethash ,(parse-integer (aref groups 0)) bots)))
            (if (is-ready source-bot)
                (progn
                  (push (reduce #'min source-bot) ,lookup-destination-low)
                  (push (reduce #'max source-bot) ,lookup-destination-high)
                  t)
                nil)))))))

(defun parse-command (line)
  (if (search "gives" line)
      (parse-gives line)
      (parse-goes-to line)))

(defun read-commands ()
  (with-open-file (f "day10.txt")
    (when f
      (loop for line = (read-line f nil nil)
         while line
         collect (parse-command line)))))

(defun circle-length (l)
  (loop for rest = (cdr l) then (cdr rest)
     for len = 1 then (1+ len)
     until (eq rest l)
     finally (return len)))

(defun test-1 ()
  (let ((commands (mapcar #'parse-command (list "value 5 goes to bot 2"
                                                "bot 2 gives low to bot 1 and high to bot 0"
                                                "value 3 goes to bot 1"
                                                "bot 1 gives low to output 1 and high to bot 0"
                                                "bot 0 gives low to output 2 and high to output 0"
                                                "value 2 goes to bot 2")))
        (bots (make-hash-table))
        (outputs (make-hash-table)))
    (process-commands commands bots outputs)))

(defun process-commands (commands bots outputs)
  (let ((cpy (copy-seq commands)))
    (setf (cdr (last cpy)) cpy)
    (setf commands cpy)
    (setf *print-circle* t))
  (loop for rest = commands then (cdr rest)
     for command = (cadr rest)
     until (null (car rest))
     when (funcall command bots outputs)
     do (setf (cadr rest) nil)
       (setf (cdr rest) (cddr rest))))

(defun part-1-solution ()
  (let ((bots (make-hash-table))
        (outputs (make-hash-table))
        (commands (read-commands)))
    (process-commands commands bots outputs)
    (loop for bot being the hash-keys of bots using (hash-value vals)
       when (and (find 61 vals) (find 17 vals))
       do (return bot))))

(defun part-2-solution ()
  (let ((bots (make-hash-table))
        (outputs (make-hash-table))
        (commands (read-commands)))
    (process-commands commands bots outputs)
    (* (car (gethash 0 outputs))
       (car (gethash 1 outputs))
       (car (gethash 2 outputs)))))
