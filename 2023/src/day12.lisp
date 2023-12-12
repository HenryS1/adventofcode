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
        (cache (make-hash-table :test 'equal)))
    (labels ((rec (remaining-springs remaining-broken)
               (aif (gethash (cons remaining-springs remaining-broken) cache)
                    it
                    (let ((result
                            (cond ((and (null remaining-broken)
                                        (null remaining-springs)) 1)
                                  ((null remaining-springs) 0)
                                  ((null remaining-broken)
                                   (if (char/= (car remaining-springs) #\#)
                                       (rec (cdr remaining-springs) remaining-broken)
                                       0))
                                  ((char= (car remaining-springs) #\.) 
                                   (rec (cdr remaining-springs) remaining-broken))
                                  ((char= (car remaining-springs) #\#)
                                   (loop for next-remaining = remaining-springs
                                           then (cdr next-remaining)
                                         for spring = (car next-remaining)
                                         for i from 1 to (car remaining-broken)
                                         when (or (null spring) (char= spring #\.))
                                           do (return 0)
                                         finally (return 
                                                   (if 
                                                    (or (null next-remaining) 
                                                        (char/= (car next-remaining) #\#))
                                                    (rec (cdr next-remaining) 
                                                         (cdr remaining-broken))
                                                    0))))
                                  (t (loop for next-remaining = remaining-springs
                                             then (cdr next-remaining)
                                           for spring = (car next-remaining)
                                           for i from 1 to (car remaining-broken)
                                           when (or (null spring) (char= spring #\.))
                                             do (return (rec (cdr remaining-springs)
                                                             remaining-broken))
                                           finally (return (if (and next-remaining
                                                                    (char= 
                                                                     (car next-remaining) #\#))
                                                               (rec (cdr remaining-springs)
                                                                    remaining-broken)
                                                               (+ (rec (cdr next-remaining) 
                                                                       (cdr remaining-broken))
                                                                  (rec (cdr remaining-springs)
                                                                       remaining-broken)))))))))
                      (setf (gethash (cons remaining-springs remaining-broken)
                                     cache) result)))))
      (rec springs broken))))

(defun read-condition-records ()
  (parse-file "input12" (sep-by (parse-condition-record) (many1 #'newlinep))))

(defun part1 ()
  (let* ((condition-records (read-condition-records))
         (ways (mapcar #'count-arrangements condition-records)))
    (reduce #'+ ways)))

(defun make-copies (condition-record)
  (bind ((springs (condition-record-springs condition-record))
         (new-springs (append springs (apply #'append 
                                             (loop for i from 1 to 4 collect 
                                                                     (cons #\? springs)))))
         (new-broken (apply #'append (loop for i from 1 to 5 
                                           collect (condition-record-broken condition-record)))))
    (make-condition-record :springs new-springs :broken new-broken)))

(defun part2 ()
  (let* ((condition-records (read-condition-records))
         (ways (mapcar #'count-arrangements (mapcar #'make-copies condition-records))))
    (reduce #'+ ways)))

(neat-lambda:disable-lambda-syntax)
(currying:disable-currying-syntax)
