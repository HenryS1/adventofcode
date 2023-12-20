(defpackage :day20
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

(in-package :day20)

(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defstruct module name type destinations state)

(defun determine-name-and-type (name)
  (cond ((char= (aref name 0) #\%) (cons (subseq name 1) 'flip-flop))
        ((char= (aref name 0) #\&) (cons (subseq name 1) 'conjunction))
        ((string= name "broadcaster") (cons name 'broadcast))
        (t (cons name 'untyped))))

(defun parse-module ()
  (sequential (raw-name (many1 #l(not (char= %c #\space))))
              (_ (seq " -> "))
              (destinations (sep-by (many1 #'alphanumericp) (seq ", ")))
              (bind (((name . type) (determine-name-and-type raw-name)))
                (make-module :name name :type type :destinations destinations
                             :state (case type
                                      (flip-flop 'off)
                                      (conjunction (make-hash-table :test 'equal)))))))

(defun setup-connections (modules)
  (let ((module-map (make-hash-table :test 'equal)))
    (loop for module in modules
          do (setf (gethash (module-name module) module-map) module))
    (loop for module in modules
          do (loop for destination in (module-destinations module)
                   for destination-module = (gethash destination module-map)
                   when (and destination-module 
                             (equal (module-type destination-module) 
                                    'conjunction))
                     do (setf (gethash (module-name module) 
                                       (module-state destination-module))
                              'low-pulse)))
    module-map))

(defun parse-modules ()
  (sep-by (parse-module) (one #'newlinep)))

(defun read-modules-from-file (filename)
  (parse-file filename (parse-modules)))

(defun process-signal-for-flip-flop (signal module)
  (let ((current-state (module-state module)))
    (case signal
      (high-pulse)
      (low-pulse (cond ((equal current-state 'off) 
                        (setf (module-state module) 'on)
                        'high-pulse)
                       ((equal current-state 'on)
                        (setf (module-state module) 'off)
                        'low-pulse)
                       (t (error "Unexpected state")))))))

(defun process-signal-for-conjunction (signal origin module)
  (setf (gethash origin (module-state module)) signal)
  (loop for received-signal being the hash-values of (module-state module)
        when (not (equal received-signal 'high-pulse))
          do (return 'high-pulse)
        finally (return 'low-pulse)))

(defstruct command signal origin destination)

(defun press-button (modules i)
;  (format t "PRESS BUTTON~%")
  (let ((q (make-queue))
        (low-count 1)
        (high-count 0)
        (rx-low-pulse-count 0)
        (broadcast (gethash "broadcaster" modules)))
    (loop for destination in (module-destinations broadcast)
          do (enqueue (make-command :signal 'low-pulse 
                                    :origin "broadcaster"
                                    :destination destination)
                      q))
    (loop for next-command = (poll q)
          while next-command 
          for command-destination = (gethash (command-destination next-command)
                                             modules
                                             "output")
          ;; do (format t "next-command ~a ~a~%" 
          ;;            (command-signal next-command)
          ;;            (command-destination next-command))
          when (and (equal (command-destination next-command) "lb")
                    (equal (command-signal next-command) 'high-pulse))
            do (format t "origin ~a i ~a~%" (command-origin next-command) i)
               ;; (when (equal (command-signal next-command) 'low-pulse)
               ;;   (incf rx-low-pulse-count))
          if (equal command-destination "output")
            do (case (command-signal next-command) 
                 (low-pulse (incf low-count))
                 (high-pulse (incf high-count)))
          else do (let ((next-signal (case (module-type command-destination)
                                       (flip-flop (process-signal-for-flip-flop 
                                                   (command-signal next-command)
                                                   command-destination))
                                       (conjunction (process-signal-for-conjunction
                                                     (command-signal next-command)
                                                     (command-origin next-command)
                                                     command-destination))))) 
                    (case (command-signal next-command)
                            (low-pulse (incf low-count))
                            (high-pulse (incf high-count)))
                    (when next-signal 
                      (loop for destination in (module-destinations command-destination)
                           do (enqueue (make-command :signal next-signal
                                                     :origin (module-name command-destination)
                                                     :destination destination)
                                       q)))))
    (list low-count high-count rx-low-pulse-count)))

(defun part1 ()
  (bind ((modules (setup-connections (read-modules-from-file "input20"))))
    (loop for i from 1 to 1000
          for (low high saw-rx) = (press-button modules i)
          summing low into low-count
          summing high into high-count
          finally (return (* low-count high-count)))))

(defun part2 ()
  (bind ((modules (setup-connections (read-modules-from-file "input20"))))
    (loop for i from 1 to 10000
          for (low high rx-low-pulse-count) = (press-button modules i)
          when (>= rx-low-pulse-count 1)
            do (return i))))

(neat-lambda:disable-lambda-syntax)
(currying:disable-currying-syntax)
