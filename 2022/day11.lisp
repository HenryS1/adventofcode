(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :cl-ppcre)
  (ql:quickload :iterate)
  (ql:quickload :anaphora)
  (ql:quickload :metabang-bind)
  (ql:quickload :alexandria)
  (ql:quickload :trivia)
  (ql:quickload :trivia.ppcre)
  (load "../2018/queue.lisp")
  (load "../2018/priority-queue.lisp")) 

(defpackage :day11
  (:use :cl :cl-ppcre :trivia trivia.ppcre :iterate :alexandria :anaphora :metabang-bind))

(in-package :day11)

(defun read-lines ()
  (iter (for line in-file "day11.input" using #'read-line)
    (when (> (length line) 0) (collect line))))

(defun take-drop (n l)
  (if (or (= n 0) (null l)) 
      (cons nil l)
      (bind (((hd . tl) (take-drop (- n 1) (cdr l))))
        (cons (cons (car l) hd) tl))))

(defun make-queue (l)
  (cons (reverse l) nil))

(defun enqueue (e q)
  (bind (((back . front) q))
    (cons (cons e back) front)))

(defun dequeue (q)
  (bind (((back . front) q))
    (if (null front)
        (let ((new-front (reverse back)))
          (cons (car new-front) (cons nil (cdr new-front))))
        (cons (car front) (cons back (cdr front))))))

(defun monkey-lcm (lines)
  (iter (for line in lines)
    (for div = (match line ((ppcre "\\s*Test: divisible by (\\d+)" (read d)) d)))
    (when div
      (reducing div by #'lcm))))

(defun read-monkey (lines &key (div nil))
  (let (*read-eval*)
    (match lines 
      ((list id-str items-str op-str test-str if-true-str if-false-str)
       (let  ((id (match id-str ((ppcre "\\s*Monkey (\\d+):" (read id-result)) id-result)))
              (items (match items-str 
                       ((ppcre "\\s*Starting items: (.*)" items-result)
                        (mapcar #'parse-integer (cl-ppcre:split ", " items-result)))))
              (op-parts (match op-str 
                          ((ppcre "\\s*Operation: new = ([^\\s]+) ([^\\s]) ([^\\s]+)"
                                  (read one) (read op) (read other))
                           `(,op ,one ,other))))
              (test-div (match test-str 
                          ((ppcre "\\s*Test: divisible by (\\d+)" (read divisor)) divisor)))
              (if-true (match if-true-str 
                         ((ppcre "\\s*If true: throw to monkey (\\d+)" (read true-monkey))
                          true-monkey)))
              (if-false (match if-false-str
                          ((ppcre "\\s*If false: throw to monkey (\\d+)" 
                                  (read false-monkey))
                           false-monkey))))
         (list id (make-queue items) 0
               (let ((worry-calc (if (null div)
                                     `(floor ,op-parts 3)
                                     `(mod ,op-parts ,div))))
                 (eval (with-gensyms (worry)
                    `(lambda (old) 
                       (let ((,worry ,worry-calc))
                         (if (= (mod ,worry ,test-div) 0)
                             (cons ,worry ,if-true)
                             (cons ,worry ,if-false)))))))))))))

(defun read-monkeys (lines &key (div nil))
  (labels ((rec (lines)
             (when lines
               (bind (((first . rest) (take-drop 6 lines)))
                 (cons (read-monkey first :div div) (rec rest))))))
    (coerce (rec lines) 'vector)))

(defun throw-item (current monkeys)
  (match current
    ((list id items inspect-count op)
     (bind (((item . new-items) (dequeue items))
            (new-current (list id new-items (+ inspect-count 1) op))
            ((new-worry . destination-id) (funcall op item))
            (destination (aref monkeys destination-id)))
       (match destination 
         ((list dest-id dest-items dest-inspect-count dest-op)
          (let ((new-destination (list dest-id (enqueue new-worry dest-items)
                                       dest-inspect-count
                                       dest-op)))
            (setf (aref monkeys id) new-current)
            (setf (aref monkeys dest-id) new-destination)
            monkeys)))))))

(defun q-empty (q)
  (and (null (car q)) (null (cdr q))))

(defun monkey-turn (current monkeys)
  (match current 
    ((list id items _ _)
     (if (q-empty items)
         (cons (+ id 1) monkeys)
         (progn (throw-item current monkeys)
                (monkey-turn (aref monkeys id) monkeys))))))

(defun monkey-round (monkeys)
  (iter (for i from 0 to (- (length monkeys) 1))
    (monkey-turn (aref monkeys i) monkeys))
  monkeys)

(defun monkey-rounds (monkeys count)
  (if (= count 0)
      monkeys
      (monkey-rounds (monkey-round monkeys) (- count 1))))

(defun take (n l)
  (subseq l 0 n))

(defun part1 ()
  (reduce #'* (take 2 (sort (map 'vector 
                                 (lambda (monkey) 
                                   (match monkey 
                                     ((list _ _ inspect-count _) inspect-count)))
                                 (monkey-rounds (read-monkeys (read-lines)) 20)) #'>))))

(defun part2 ()
  (let ((lines (read-lines)))
    (reduce #'* (take 2 (sort (map 'vector 
                                 (lambda (monkey) 
                                   (match monkey 
                                     ((list _ _ inspect-count _) inspect-count)))
                                 (monkey-rounds
                                  (read-monkeys lines 
                                                :div (monkey-lcm lines)) 10000)) #'>)))))
