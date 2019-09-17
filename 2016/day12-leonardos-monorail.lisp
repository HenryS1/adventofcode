(ql:quickload :cl-ppcre)

(declaim (optimize (speed 3) (debug 0)))

(defparameter *registers* (make-array 4 :element-type 'integer :initial-element 0))
(defparameter *instruction-pointer* 0)

(defun translate-sym (s)
  (case s
    (a 0)
    (b 1)
    (c 2)
    (d 3)
    (otherwise s)))

(defun generate-copy (instruction-index args)
  (let ((translated-syms (mapcar #'translate-sym args)))
    (if (symbolp (car args))
        `(,instruction-index
           (progn (setf (aref *registers* ,(cadr translated-syms)) 
                        (aref *registers* ,(car translated-syms)))
                  (incf *instruction-pointer*)))
        `(,instruction-index
          (progn (setf (aref *registers* ,(cadr translated-syms))
                       ,(car translated-syms))
                 (incf *instruction-pointer*))))))

(defun generate-inc (instruction-index args)
  `(,instruction-index
     (progn (incf (aref *registers* ,(translate-sym (car args))))
            (incf *instruction-pointer*))))

(defun generate-dec (instruction-index args)
  `(,instruction-index
    (progn (decf (aref *registers* ,(translate-sym (car args))))
           (incf *instruction-pointer*))))

(defun generate-jnz (instruction-index args)
  (if (symbolp (car args))
      `(,instruction-index
         (if (/= 0 (aref *registers* ,(translate-sym (car args))))
             (incf *instruction-pointer* ,(cadr args))
             (incf *instruction-pointer*)))
      `(,instruction-index
         (if (/= 0 ,(car args))
             (incf *instruction-pointer* ,(cadr args))
             (incf *instruction-pointer*)))))

(defun generate-instruction (instruction-index syms)
  (case (car syms)
    (cpy (generate-copy instruction-index (cdr syms)))
    (inc (generate-inc instruction-index (cdr syms)))
    (dec (generate-dec instruction-index (cdr syms)))
    (jnz (generate-jnz instruction-index (cdr syms)))
    (otherwise (error (format nil "unknown instruction name ~a" (car syms))))))

(defun read-syms (line)
  (let (*read-eval*)
    (with-input-from-string (s line)
      (loop for sym = (read s nil nil)
         while sym collect sym))))

(defun read-instructions ()
  (with-open-file (f "day12-input")
    (when f
      (let ((instructions 
             (loop for line = (read-line f nil nil)
                while line 
                for syms = (read-syms line)
                for instruction-index = 0 then (+ 1 instruction-index)
                collect (generate-instruction instruction-index syms))))
        `(loop while (< *instruction-pointer* ,(length instructions))
            do ,(append '(case *instruction-pointer*) instructions)
            finally (return (aref *registers* 0)))))))

(defun def-answer1 ()
  (compile (eval `(defun answer1 ()
            (setf *instruction-pointer* 0)
            (fill *registers* 0)
            ,(read-instructions)))))

(defun def-answer2 ()
  (compile (eval 
            `(defun answer2 ()
               (setf *instruction-pointer* 0)
               (fill *registers* 0)
               (setf (aref *registers* 2) 1)
               ,(read-instructions)))))
