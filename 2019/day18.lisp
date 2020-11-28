(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :cl-ppcre)
  (ql:quickload :iterate)
  (ql:quickload :anaphora)
  (ql:quickload :metabang-bind)
  (ql:quickload :alexandria)
  (ql:quickload :cl-arrows)
  (load "../2018/queue.lisp")
  (load "../2018/priority-queue.lisp")) 

(defpackage :day18
  (:use :cl :cl-ppcre :cl-arrows :iterate :alexandria :anaphora :metabang-bind))

(in-package :day18)

(defun ints (line) 
  (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" line)))

(defun read-syms (line &optional (sep " "))
  (let (*read-eval*)
    (mapcar #'read-from-string (split sep line))))

(defun read-map (file)
  (iter (for line in-file file using #'read-line)
        (collect line result-type 'vector)))

(defun locations (mp)
  (iter outer
        (for r from 0 to (- (length mp) 1))
        (iter (for c from 0 to (- (length (aref mp 0)) 1))
              (for ch = (aref (aref mp r) c))
              (when (or (alpha-char-p ch)
                        (char= ch #\@) (char= ch #\!) 
                        (char= ch #\&) (char= ch #\?))
                (in outer (collect (cons ch (cons r c))))))))

(defun find-distances (locations mp)
  (let ((distances (make-hash-table)))
    (labels ((neighbours (r c seen callback)
               (iter (for row from (- r 1) to (+ r 1))
                     (iter (for col from (- c 1) to (+ c 1))
                           (for coord = (cons row col))
                           (when (and (not (gethash coord seen))
                                      (or (/= r row) (/= c col))
                                      (or (= r row) (= c col))
                                      (<= 0 row (- (length mp) 1))
                                      (<= 0 col (- (length (aref mp 0)) 1))
                                      (let ((ch (aref (aref mp row) col)))
                                        (or (char= ch #\.) (char= ch #\@)
                                            (char= ch #\!) (char= ch #\&)
                                            (char= ch #\?)(alpha-char-p ch))))
                             (funcall callback coord)))))
             (bfs (letter coord seen)
               (iter (with q = (make-queue (cons 0 coord)))
                     (while (non-empty q))
                     (for (dist . cur) = (poll q))
                     (for (r . c) = cur)
                     (setf (gethash cur seen) t)
                     (for ch = (aref (aref mp r) c))
                     (if (or (equal cur coord) (char= ch #\.) (char= ch #\@)
                             (char= ch #\!) (char= ch #\&) (char= ch #\?))
                         (neighbours r c seen
                                     (lambda (nbr)                                       
                                       (setf (gethash nbr seen) t)
                                       (enqueue (cons (+ dist 1) nbr) q)))
                         (push (cons ch dist) (gethash letter distances))))))
      (iter (for (letter . coord) in locations)
            (bfs letter coord (make-hash-table :test 'equal)))
      distances)))

(defun translate-char (ch num-keys)
  (let ((result (cond ((char= ch #\@) (+ (* 2 num-keys) 0))
                      ((char= ch #\!) (+ (* 2 num-keys) 1))
                      ((char= ch #\&) (+ (* 2 num-keys) 2))
                      ((char= ch #\?) (+ (* 2 num-keys) 3))
                      ((upper-case-p ch) (+ num-keys (- (char-code ch) 65)))
                      (t (- (char-code ch) 97)))))
    result))

(declaim (inline get-seen encode-dist decode-bits with-dist get-position available get-state
                 get-dist make-state all-visited))

(defun encode-dist (pair sep-offset num-keys)
  (+ (ash (cdr pair) sep-offset) (translate-char (car pair) num-keys)))

(defun decode-bits (bits sep-offset loc-mask)
  (values (ash bits (- sep-offset)) (logand bits loc-mask)))

(defun bittify (distances sep-offset num-keys)
  (iter (with bitted = (make-hash-table))
        (for (n dists) in-hashtable distances)
        (for new-dist = (make-array (length dists) :element-type 'fixnum 
                                    :initial-contents 
                                    (mapcar (lambda (pr)
                                              (encode-dist pr sep-offset num-keys)) dists)))
        (setf (gethash (translate-char n num-keys) bitted) new-dist)
        (finally (return bitted))))

(defun get-seen (state seen-mask) (logand state seen-mask))
(defun with-dist (state dist dist-offset) (logior state (ash dist dist-offset)))
(defun get-position (state position-offset) (ash state (- position-offset)))
(defun available (pos seen num-keys) (or (< pos num-keys) (> (logand (ash 1 pos) seen) 0)))
(defun get-state (state-dist state-mask) (logand state-dist state-mask))
(defun get-dist (state-dist dist-offset) (ash state-dist (- dist-offset)))
(defun dist-comp (dist-offset) (lambda (a b) (< (get-dist a dist-offset) (get-dist b dist-offset))))
(defun make-state (position seen position-offset)
  (logior (ash position position-offset) (logior (ash 1 position) seen)))
(defun all-visited (state keys-mask) (= (logand state keys-mask) keys-mask))

(defun make-seen-mask (num-keys) (- (ash 1 (* num-keys 2)) 1))
(defun make-keys-mask (num-keys) (- (ash 1 num-keys) 1))
(defun find-sep-offset (num-keys) 
  (iter (for offset from 1)
        (for cur first 1 then (ash cur 1))
        (while (< cur (* num-keys 2)))
        (finally (return offset))))
(defun find-loc-mask (num-keys)
  (- (ash 1 (find-sep-offset num-keys)) 1))
(defun find-dist-offset (num-keys) 
  (+ (* num-keys 2) (find-sep-offset num-keys)))
(defun find-position-offset (num-keys) (* 2 num-keys))
(defun make-state-mask (num-keys) (- (ash 1 (find-dist-offset num-keys)) 1))

(defconstant inf (- (expt 2 32) 1))

(defun explore (distances num-keys)
  (let* ((seen-mask (make-seen-mask num-keys))
         (keys-mask (make-keys-mask num-keys))
         (state-mask (make-state-mask num-keys))
         (dist-offset (find-dist-offset num-keys))
         (position-offset (find-position-offset num-keys))
         (sep-offset (find-sep-offset num-keys))
         (loc-mask (find-loc-mask num-keys))
         (dist-to (make-hash-table))
         (pq (make-pq (dist-comp dist-offset))))
    (iter (for nbr in-vector (gethash (* 2 num-keys) distances))
          (bind (((:values sep loc) (decode-bits nbr sep-offset loc-mask))
                 (new-state (make-state loc 0 position-offset)))
            (when (< loc num-keys)
             (setf (gethash new-state dist-to) sep)
             (insert-pq (with-dist new-state sep dist-offset) pq))))
    (iter (while (pq-nonempty pq))
          (for state-dist = (pop-pq pq))
          (for state = (get-state state-dist state-mask))
          (when (all-visited state keys-mask)
            (return (get-dist state-dist dist-offset)))
          (for position = (get-position state position-offset))
          (iter (for nbr in-vector (gethash position distances))
                (bind (((:values sep loc) (decode-bits nbr sep-offset loc-mask))
                       (new-state (make-state loc (get-seen state seen-mask) position-offset))
                       (new-dist (+ sep (get-dist state-dist dist-offset))))
                  (when (and (< new-dist (gethash new-state dist-to inf))
                             (or (< loc num-keys) (> (logand (ash 1 (- loc num-keys)) new-state) 0)))
                    (setf (gethash new-state dist-to) new-dist)
                    (insert-pq (with-dist new-state new-dist dist-offset) pq)))))))

(defun count-keys (mp)
  (iter (for line in-vector mp)
        (summing (count-if #'lower-case-p line))))

(defun solve (mp)
  (let* ((locs (locations mp))
         (num-keys (count-keys mp))
         (distances (-<> (find-distances locs mp) (bittify <> (find-sep-offset num-keys) num-keys))))
    (explore distances num-keys)))

(defun test-1 ()
  (solve #("########################"
           "#f.D.E.e.C.b.A.@.a.B.c.#"
           "######################.#"
           "#d.....................#"
           "########################")))

(defun test-2 ()
  (solve #("#########"
           "#b.A.@.a#"
           "#########")))

(defun test-3 ()
  (solve #("########################"
           "#...............b.C.D.f#"
           "#.######################"
           "#.....@.a.B.c.d.A.e.F.g#"
           "########################")))

(defun test-4 ()
  (solve #("#################"
           "#i.G..c...e..H.p#"
           "########.########"
           "#j.A..b...f..D.o#"
           "########@########"
           "#k.E..a...g..B.n#"
           "########.########"
           "#l.F..d...h..C.m#"
           "#################")))

(defun test-5 ()
  (solve #("########################"
           "#@..............ac.GI.b#"
           "###d#e#f################"
           "###A#B#C################"
           "###g#h#i################"
           "########################")))

(defun part-1 ()
  (solve (read-map "input18")))

(defun find-sep-offset-par (num-keys) 
  (iter (for offset from 0)
        (for cur first 1 then (ash cur 1))
        (while (< cur (+ (* num-keys 2) 4)))
        (finally (return offset))))

(defun find-dist-offset-par (num-keys) 
  (+ (* num-keys 2) (* 4 (find-sep-offset num-keys))))

(defun make-state-mask-par (num-keys) (- (ash 1 (find-dist-offset-par num-keys)) 1))

(defun find-position-size (num-keys)
  (find-sep-offset-par num-keys))

(defun find-position-mask (num-keys)
  (- (ash 1 (find-position-size num-keys)) 1))

(defun get-positions-par (state position-offset position-mask position-size) 
  (let ((positions (ash state (- position-offset))))
    (values (logand position-mask positions)
            (logand position-mask (ash positions (- position-size)))
            (logand position-mask (ash positions (- (* position-size 2))))
            (logand position-mask (ash positions (- (* position-size 3)))))))

(defun make-state-par (position seen seen-mask old-positions position-offset position-index position-mask position-size)
  (let ((shft (* position-size position-index)))
    (-<> (logxor old-positions (logand old-positions (ash position-mask shft)))
         (logxor <> (ash position shft))
         (ash <> position-offset)
         (logior (logand seen-mask (logior seen (ash 1 position))) <>))))

(defun get-all-positions (state position-offset)
  (ash state (- position-offset)))

(defun init-positions (num-keys position-size)
  (logior (* num-keys 2)
          (ash (+ 1 (* num-keys 2)) position-size)
          (ash (+ 2 (* num-keys 2)) (* position-size 2))
          (ash (+ 3 (* num-keys 2)) (* position-size 3))))

(defun explore-par (distances num-keys)
  (let* ((seen-mask (make-seen-mask num-keys))
         (keys-mask (make-keys-mask num-keys))
         (state-mask (make-state-mask-par num-keys))
         (dist-offset (find-dist-offset-par num-keys))
         (position-offset (find-position-offset num-keys))
         (position-mask (find-position-mask num-keys))
         (position-size (find-position-size num-keys))
         (sep-offset (find-sep-offset num-keys))
         (loc-mask (find-loc-mask num-keys))
         (dist-to (make-hash-table))
         (pq (make-pq (dist-comp dist-offset))))
    (iter (with init-pos = (init-positions num-keys position-size))
          (for strt in-vector #(0 1 2 3))
          (iter (for nbr in-vector (gethash (+ (* num-keys 2) strt) distances))
                (bind (((:values sep loc) (decode-bits nbr sep-offset loc-mask))
                       (new-state (make-state-par loc 0 seen-mask init-pos position-offset strt
                                                  position-mask position-size)))
                  (when (< loc num-keys)
                    (setf (gethash new-state dist-to) sep)
                    (insert-pq (with-dist new-state sep dist-offset) pq)))))
    (labels ((enqueue-nbrs (position position-index state state-dist)
               (iter (for nbr in-vector (gethash position distances))
                     (bind (((:values sep loc) (decode-bits nbr sep-offset loc-mask))
                            (new-state (make-state-par loc (get-seen state seen-mask) seen-mask 
                                                       (get-all-positions state position-offset)
                                                       position-offset position-index
                                                       position-mask position-size))
                            (new-dist (+ sep (get-dist state-dist dist-offset))))
                      (when (and (< new-dist (gethash new-state dist-to inf))
                                 (or (< loc num-keys) 
                                     (> (logand (ash 1 (- loc num-keys)) new-state) 0)))
                        (setf (gethash new-state dist-to) new-dist)
                        (insert-pq (with-dist new-state new-dist dist-offset) pq))))))
      (iter (while (pq-nonempty pq))
            (for state-dist = (pop-pq pq))
            (for state = (get-state state-dist state-mask))
            (when (all-visited state keys-mask)
              (return (get-dist state-dist dist-offset)))
            (multiple-value-bind (p1 p2 p3 p4) (get-positions-par state position-offset 
                                                                  position-mask position-size)
              (enqueue-nbrs p1 0 state state-dist) (enqueue-nbrs p2 1 state state-dist)
              (enqueue-nbrs p3 2 state state-dist) (enqueue-nbrs p4 3 state state-dist))))))

(defun solve-par (mp)
  (let* ((locs (locations mp))
         (num-keys (count-keys mp))
         (distances (-<> (find-distances locs mp) (bittify <> (find-sep-offset num-keys) num-keys))))
    (explore-par distances num-keys)))

(defun test-par-1 ()
  (solve-par #("#######"
               "#a.#Cd#"
               "##@#!##"
               "#######"
               "##&#?##"
               "#cB#Ab#"
               "#######")))

(defun test-par-2 ()
  (solve-par #("###############"
               "#d.ABC.#.....a#"
               "######@#!######"
               "###############"
               "######&#?######"
               "#b.....#.....c#"
               "###############")))

(defun test-par-3 ()
  (solve-par #("#############"
               "#DcBa.#.GhKl#"
               "#.###@#!#I###"
               "#e#d#####j#k#"
               "###C#&#?###J#"
               "#fEbA.#.FgHi#"
               "#############")))

(defun test-par-4 ()
  (solve-par #("#############"
               "#g#f.D#..h#l#"
               "#F###e#E###.#"
               "#dCba@#!BcIJ#"
               "#############"
               "#nK.L&#?G...#"
               "#M###N#H###.#"
               "#o#m..#i#jk.#"
               "#############")))

(defun part-2 ()
  (solve-par (read-map "input18_par")))
