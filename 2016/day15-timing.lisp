(ql:quickload :cl-ppcre)

(defun sanitise (s) 
  (cl-ppcre:regex-replace-all "(?:,|;|#|\\.|=)" s ""))

(defun read-slot (offset line)
  (let (*read-eval*)
    (let ((syms (with-input-from-string (s (sanitise (subseq line 0 (length line))))
                   (loop for sym = (read s nil nil)
                      while sym collect sym))))
      (let ((fst (nth 3 syms))
            (snd (+ offset (nth 11 syms))))
        (list fst (mod snd fst))))))

(defun part-1-slots ()
  (with-open-file (f "day15-input.txt")
    (when f 
      (loop for line = (read-line f nil nil)
         for offset = 1 then (+ 1 offset)
         while line collect (read-slot offset line)))))

(defun test-slots ()
  (let ((lines (list "Disc #1 has 5 positions; at time=0, it is at position 4."
                     "Disc #2 has 2 positions; at time=0, it is at position 1.")))
    (list (read-slot 1 (car lines))
          (read-slot 2 (cadr lines)))))

(defun next-alignment (time slot)
  (let ((diff (- (car slot) (cadr slot))))
    (mod (- (car slot) (mod (- time diff) (car slot))) (car slot))))

(defun find-alignment (slots start)
  (loop with time = start
     for alignments = (mapcar (lambda (slot) (next-alignment time slot)) slots)
     while (some (lambda (a) (> a 0)) alignments)
     do (mapc (lambda (slot) (incf time (next-alignment time slot))) slots)
     finally (return time)))

(defun answer1 () (find-alignment (part-1-slots) 0))

(defun part-2-slots ()
  (let* ((slots (part-1-slots))
         (offset (length slots)))
    (nconc (last slots) (list (list 11 (mod (+ offset 1) 11))))
    slots))

(defun answer2 () (find-alignment (part-2-slots) 0))
