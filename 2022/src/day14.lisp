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
  (bind ((down (down-obstacle position obstacles)))
    (if (null down)
        (list 'finished obstacles)
        (bind (((down-x . down-y) down)
               (next-position (cons down-x (- down-y 1)))
               (left-down (left-down-obstacle next-position obstacles)))
          (if (null left-down)
              (move-sand (cons (- down-x 1) down-y) obstacles)
              (bind ((right-down (right-down-obstacle next-position obstacles)))
                (if (null right-down)
                    (move-sand (cons (+ down-x 1) down-y) obstacles)
                    (cons 'not-finished (cons (cons next-position next-position) obstacles)))))))))

(defun move-sand-rec (position obstacles sand-count)
  (bind (((x . y) position)
         (down-obstacle (down-obstacle position obstacles)))
    (if (null down-obstacle)
        (list 'finished obstacles sand-count)
        (bind (((_ . down-y) down-obstacle))
          (cond ((> down-y (+ y 1))
                 (bind ((down-result
                         (move-sand-rec (cons x (- down-y 1)) obstacles sand-count))
                        ((down-outcome down-obs down-cnt) down-result))
                   (if (equal down-outcome 'finished)
                       down-result
                       (move-sand-rec position down-obs down-cnt))))
                ((null (left-down-obstacle position obstacles))
                 (bind ((left-result
                         (move-sand-rec (cons (- x 1) (+ y 1)) obstacles sand-count))
                        ((left-outcome left-obs left-cnt) left-result))
                   (if (equal left-outcome 'finished)
                       left-result
                       (move-sand-rec position left-obs left-cnt))))
                ((null (right-down-obstacle position obstacles))
                 (bind ((right-result
                         (move-sand-rec (cons (+ x 1) (+ y 1)) obstacles sand-count))
                        ((right-outcome right-obs right-cnt) right-result))
                   (if (equal right-outcome 'finished)
                       right-result
                       (move-sand-rec position right-obs right-cnt))))
                (t ;(format t "NEW SAND~%")
                 (list 'not-finished (sort (cons (cons position position) obstacles)
                                             #'compare-obstacles) 
                         (+ sand-count 1))))))))

(defun part1 ()
  (let ((obstacles (read-lines)))
    (car (last (move-sand-rec (cons 500 0) obstacles 0)))))

(defun move-sand-pyramid (position obstacles sand-count)
  (bind (((x . y) position)
         (down-obstacle (down-obstacle position obstacles)))
    (bind (((_ . down-y) down-obstacle))
      (cond ((> down-y (+ y 1))
             (bind (((down-obs down-cnt) 
                     (move-sand-pyramid (cons x (- down-y 1)) obstacles sand-count)))
               (move-sand-pyramid position down-obs down-cnt)))
            ((null (left-down-obstacle position obstacles))
             (bind (((left-obs left-cnt)
                     (move-sand-pyramid (cons (- x 1) (+ y 1)) obstacles sand-count)))
               (move-sand-pyramid position left-obs left-cnt)))
            ((null (right-down-obstacle position obstacles))
             (bind (((right-obs right-cnt)
                     (move-sand-pyramid (cons (+ x 1) (+ y 1)) obstacles sand-count)))
               (move-sand-pyramid position right-obs right-cnt)))
            (t (list (sort (cons (cons position position) obstacles)
                           #'compare-obstacles) 
                    (+ sand-count 1)))))))

(defun max-y (obstacles)
  (reduce #'max (mapcar (lambda (o) 
                       (bind (((_ . end) o))
                         (cdr end))) obstacles)))

(defun part2 ()
  (let* ((obstacles (read-lines))
         (max-y (max-y obstacles)))
    (move-sand-pyramid 
     (cons 500 0) 
     (sort (cons (cons (cons -100000000 (+ max-y 2)) (cons 1000000000 (+ max-y 2)))
                 obstacles)
           #'compare-obstacles) 0)))

