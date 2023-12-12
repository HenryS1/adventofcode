(defpackage :day12
  (:use 
   :cl 
   :iterate 
   :alexandria 
   :anaphora 
   :pears
   :metabang-bind
   :queue)
  (:export
   :make-condition-record
   :parse-condition-record
   :count-arrangements))

(in-package :day12)

(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defstruct condition-record springs broken)

(defun parse-condition-record ()
  (sequential (springs (fmap #l(coerce %springs 'list) (many1 #p(char/= #\space))))
              (_ (many1 #p (char= #\space)))
              (broken (sep-by *non-negative-int* (many1 #p(char= #\,))))
              (make-condition-record :springs springs :broken broken)))

(defun count-arrangements (condition-record)
  (let ((springs (condition-record-springs condition-record))
        (broken (condition-record-broken condition-record))
        (count 0))
 ;   (format t "CONDITION-RECORD ~a~%" condition-record)
    (labels ((rec (remaining-springs remaining-broken)
               ;; (format t "REMAINING SPRINGS ~a REMAINING BROKEN ~a~%" remaining-springs 
               ;;         remaining-broken)
               (cond ((and (null remaining-broken) (null remaining-springs)) (incf count))
                     ((null remaining-springs) nil)
                     ((null remaining-broken) (when (char/= (car remaining-springs) #\#)
                                                (rec (cdr remaining-springs) remaining-broken)))
                     ((char= (car remaining-springs) #\.) 
                      (rec (cdr remaining-springs) remaining-broken))
                     ((char= (car remaining-springs) #\#)
                      (loop for next-remaining = remaining-springs then (cdr next-remaining)
                            for spring = (car next-remaining)
                            for i from 1 to (car remaining-broken)
                            when (or (null spring) (char= spring #\.))
                              do (return nil)
                            finally (when (or (null next-remaining) 
                                              (char/= (car next-remaining) #\#))
                                      (rec (cdr next-remaining) (cdr remaining-broken)))))
                     (t ;(format t "THIS CASE~%")
                      (loop for next-remaining = remaining-springs then (cdr next-remaining)
                            for spring = (car next-remaining)
                            for i from 1 to (car remaining-broken)
                            when (or (null spring) (char= spring #\.))
                              do (return (rec (cdr remaining-springs) remaining-broken))
                            finally (if (and next-remaining
                                             (char= (car next-remaining) #\#))
                                        (progn 
                         ;                 (format t "HERE~%")
                                          (rec (cdr remaining-springs) remaining-broken))
                                        (progn 
                          ;                (format t "HERE2~%")
                                          (rec (cdr next-remaining) (cdr remaining-broken))
                                          (rec (cdr remaining-springs) remaining-broken)))))
                     ;; (t (bind (((skipped . next-remaining)
                     ;;            (loop for next-rem = remaining-springs then (cdr next-rem)
                     ;;                  for spring = (car next-rem)
                     ;;                  for i from 1 to (car remaining-broken)
                     ;;                  for contains-broken = nil 
                     ;;                    then (or contains-broken 
                     ;;                             (equal spring #\#))
                     ;;                  do (format t "NEXT REM ~a~%" next-rem)
                     ;;                  when (or (null next-rem) 
                     ;;                           (and (char= spring #\.)
                     ;;                                (char= (car remaining-springs)  #\#)))
                     ;;                    do (return (cons t nil))
                     ;;                  when (char= spring #\.)
                     ;;                    do (return 
                     ;;                         (if contains-broken
                     ;;                             (cons t nil)
                     ;;                             (loop for rem = next-rem then (cdr rem)
                     ;;                                   for next-spring = (car rem) 
                     ;;                                   while (and next-spring 
                     ;;                                              (char= next-spring #\.))
                     ;;                                   ;;                                                         do (format t "NEXT SPRING ~a~%" next-spring)
                     ;;                                   finally (return (cons t rem)))))
                     ;;                  finally (return (if (and next-rem (char= (car next-rem) #\#))
                     ;;                                      (cons t (cdr remaining-springs))
                     ;;                                      (cons nil next-rem))))))
                     ;;      (format t "SKIPPED ~a NEXT REMAINING ~a~%" skipped next-remaining)
                     ;;      (if skipped 
                     ;;          (rec next-remaining remaining-broken)
                     ;;          (progn (rec (cdr next-remaining) (cdr remaining-broken))
                     ;;                 (when (char/= (car remaining-springs) #\#)
                     ;;                     (rec (cdr remaining-springs) remaining-broken))))))
                     )))
      (rec springs broken)
      (format t "DONE~%")
      count)))

(defun make-copies (condition-record)
  (bind ((springs (condition-record-springs condition-record))
         (new-springs (append springs (apply #'append 
                                             (loop for i from 1 to 4 collect 
                                                                     (cons #\? springs)))))
         (new-broken (apply #'append (loop for i from 1 to 5 
                                           collect (condition-record-broken condition-record)))))
    (make-condition-record :springs new-springs :broken new-broken)))

(defun read-condition-records ()
  (parse-file "input12" (sep-by (parse-condition-record) (many1 #'newlinep))))

(defun part1 ()
  (let* ((condition-records (read-condition-records))
         (ways (mapcar #'count-arrangements condition-records)))
    (reduce #'+ ways)))

(defun part2 ()
  (let* ((condition-records (read-condition-records))
         (ways (mapcar #'count-arrangements (mapcar #'make-copies condition-records))))
    (reduce #'+ ways)))

(neat-lambda:disable-lambda-syntax)
(currying:disable-currying-syntax)
