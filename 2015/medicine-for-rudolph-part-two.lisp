(load "../2018/priority-queue.lisp")
(ql:quickload :cl-ppcre)

(defun read-molecule (line)
  (let ((splt (cl-ppcre:split "\\s+" line)))
    (cons (caddr splt) (car splt))))

(defun read-problem-statement (lines)
  (loop for line in lines
     with molecules = (make-hash-table :test 'equal)
     with start = nil
     do (cond ((search "=>" line)
               (destructuring-bind (input . result) (read-molecule line)
                 (push result (gethash input molecules))))
              ((> (length line) 0) (setf start line)))
     finally (return (cons molecules start))))

(defun read-input ()
  (with-open-file (f "medicine-for-rudolph-input.txt")
    (when f
      (loop for line = (read-line f nil nil)
         while line 
         collect line into lines
         finally (return (read-problem-statement lines))))))

(defun occurrences (s str)
  (labels ((occurs-at (i)
             (loop for c across s
                for c-other = (aref str i)
                do (if (char/= c (aref str i)) 
                       (return-from occurs-at nil))
                  (incf i))
             t))
    (loop for i from 0 to (- (length str) (length s))
       when (occurs-at i)
       collect i)))

(defun replace-occ (match str i)
  (lambda (replacement)
    (concatenate 'string (subseq str 0 i) 
                 replacement
                 (subseq str (+ i (length match))))))

(defun mappend (f l)
  (loop for l in (mapcar f l) append l))

(defun candidates (molecules)
  (lambda (current)
    (loop for match being the hash-keys of molecules using (hash-value results)
       for occs = (occurrences match current)
       append (mappend (lambda (index) (mapcar (replace-occ match current index) results))
                       occs))))

(defun enqueue-candidates (q candidate-fun)
  (let ((seen (make-hash-table :test 'equal)))
    (lambda (current)
      (loop for candidate in (funcall candidate-fun (car current))
         when (not (gethash candidate seen))
         do (insert-pq (cons candidate (1+ (cdr current))) q)
           (setf (gethash candidate seen) t)))))

(defun find-target (start molecules)
  (let* ((pq (make-pq (lambda (one other) (< (length (car one)) (length (car other))))))
         (current (cons start 0))
         (enqueue-fun (enqueue-candidates pq (candidates molecules))))
    (funcall enqueue-fun current)
    (loop while (and (not (string-equal (car current) "e"))
                     (not (empty pq)))
       do (funcall enqueue-fun current)
         (setf current (pop-pq pq))
       finally (return current))))

(defun run ()
  (destructuring-bind (molecules . start) (read-input)
    (find-target target molecules)))
