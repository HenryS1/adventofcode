(defpackage :day14
  (:use 
   :cl 
   :aoc.functional
   :cl-ppcre 
   :trivia 
   :trivia.ppcre 
   :iterate 
   :arrow-macros
   :alexandria 
   :anaphora 
   :metabang-bind
   :aoc.datastructures)
  (:shadowing-import-from #:arrow-macros #:<>))

(in-package :day14)

(defun groups-of-2 (ns)
  (when ns
    (bind (((group . rest) (take-drop 2 ns)))
      (cons (cons (car group) (cadr group)) (groups-of-2 rest)))))

(defun tuple-less (one other)
  (bind (((one-x . one-y) one)
         ((other-x . other-y) other))
    (or (< one-x other-x)
        (and (= one-x other-x)
             (< one-y other-y)))))

(defun group-boundaries (boundaries)
  (if (null (cdr boundaries))
      nil
      (let ((one (car boundaries))
            (other (cadr boundaries)))
        (if (tuple-less one other)
            (cons (cons one other)
                  (group-boundaries (cdr boundaries)))
            (cons (cons other one)
                  (group-boundaries (cdr boundaries)))))))

(defun parse-boundaries (line)
  (-<> (regex-replace-all "," line " ")
    (regex-replace-all " -> " <> " ")
    (split "\\s+" <>)
    (map 'list #'parse-integer <>)
    (groups-of-2 <>)
    (group-boundaries <>)))

(defun compare-obstacles (a b)
  (bind (((a-start . _) a)
         ((b-start . _) b))
    (< (cdr a-start) (cdr b-start))))

(defun read-lines ()
  (sort (iter outer (for line in-file "day14.input" using #'read-line)
          (iter (for boundary in (parse-boundaries line))
            (in outer (collect boundary))))
        #'compare-obstacles))

(defun down-obstacle (position obstacles)
  (bind (((x . y) position))
    (let ((result (find-if (lambda (obstacle)
;                             (format t "CHECKING OBSTACLE ~a~%" obstacle)
                      (bind (((obs-start . obs-end) obstacle)
                             ((start-x . start-y) obs-start)
                             ((end-x . _) obs-end))
                        (and (<= start-x x end-x)
                             (< y start-y))))
                    obstacles)))
      (if result
          (bind ((((_ . start-y) . _) result))
            (cons x start-y))))))

(defun left-down-obstacle (position obstacles)
  (bind (((x . y) position))
    (find-if (lambda (obstacle)
               (bind (((obs-start . obs-end) obstacle)
                      ((start-x . start-y) obs-start)
                      ((end-x . end-y) obs-end))
                 (or (and (<= start-x (- x 1) end-x) (= start-y (+ y 1) end-y))
                     (and (<= start-y (+ y 1) end-y) (= start-x (- x 1) end-x)))))
             obstacles)))

(defun right-down-obstacle (position obstacles)
  (bind (((x . y) position))
    (find-if (lambda (obstacle)
               (bind (((obs-start . obs-end) obstacle)
                      ((start-x . start-y) obs-start)
                      ((end-x . end-y) obs-end))
                 (or (and (<= start-x (+ x 1) end-x) (= start-y (+ y 1) end-y))
                     (and (<= start-y (+ y 1) end-y) (= start-x (+ x 1) end-x)))))
             obstacles)))

(defun move-sand (position obstacles)
;  (format t "POSITION ~a~%" position)
  (bind ((down (down-obstacle position obstacles)))
;    (format t "DOWN ~a~%" down)
    (if (null down)
        (cons 'finished obstacles)
        (bind (((down-x . down-y) down)
               (next-position (cons down-x (- down-y 1)))
               (left-down (left-down-obstacle next-position obstacles)))
;          (format t "NEXT-POSITION ~a~%" next-position)
;          (format t "LEFT DOWN ~a~%" left-down)
          (if (null left-down)
              (move-sand (cons (- down-x 1) down-y) obstacles)
              (bind ((right-down (right-down-obstacle next-position obstacles)))
;                (format t "RIGHT DOWN ~a~%" right-down)
                (if (null right-down)
                    (move-sand (cons (+ down-x 1) down-y) obstacles)
                    (cons 'not-finished (cons (cons next-position next-position) obstacles)))))))))

(defun move-sand-until-finished (start obstacles sand-count)
;  (format t "OBSTACLES~%~a~%" obstacles)
  (bind (((result . new-obstacles) (move-sand start obstacles)))
    (if (equal result 'finished)
        (- sand-count 1)
        (move-sand-until-finished start (sort new-obstacles #'compare-obstacles) 
                                  (+ sand-count 1))))
 ;; (if (= sand-count 25)
 ;;      obstacles
 ;; )
  )

(defun part1 ()
  (let ((obstacles (read-lines)))
    (move-sand-until-finished (cons 500 0) obstacles 1)))

(defun max-y (obstacles)
  (reduce #'max (mapcar (lambda (o) 
                       (bind (((_ . end) o))
                         (cdr end))) obstacles)))

(defun move-sand-until-at-top (start obstacles sand-count)
;  (format t "OBSTACLES~%~a~%" obstacles)
  (bind (((_ . new-obstacles) (move-sand start obstacles)))
    (if (find '((500 . 0) . (500 . 0)) new-obstacles :test 'equal)
        (- sand-count 1)
        (move-sand-until-at-top start (sort new-obstacles #'compare-obstacles) 
                                (+ sand-count 1))))
 ;; (if (= sand-count 25)
 ;;      obstacles
 ;; )
  )

(defun part2 ()
  (let* ((obstacles (read-lines))
         (max-y (max-y obstacles)))
    (move-sand-until-at-top (cons 500 0) (cons (cons (cons -1000000 (+ max-y 2)) (cons 10000000 (+ max-y 2)))
                                               obstacles) 1)))
