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

(defun generate-copy (args)
  (let ((translated-syms (mapcar #'translate-sym args)))
    (if (symbolp (car args))
        `(lambda ()
           (setf (aref *registers* ,(cadr translated-syms)) 
                 (aref *registers* ,(car translated-syms)))
           (incf *instruction-pointer*))
        `(lambda ()
           (setf (aref *registers* ,(cadr translated-syms))
                 ,(car translated-syms))
           (incf *instruction-pointer*)))))

(defun generate-inc (args)
  `(lambda ()
     (incf (aref *registers* ,(translate-sym (car args))))
     (incf *instruction-pointer*)))

(defun generate-dec (args)
  `(lambda ()
     (decf (aref *registers* ,(translate-sym (car args))))
     (incf *instruction-pointer*)))

(defun generate-jnz (args)
  (if (symbolp (car args))
      `(lambda ()
         (if (/= 0 (aref *registers* ,(translate-sym (car args))))
             (incf *instruction-pointer* ,(cadr args))
             (incf *instruction-pointer*)))
      `(lambda ()
         (if (/= 0 ,(car args))
             (incf *instruction-pointer* ,(cadr args))
             (incf *instruction-pointer*)))))

(defun generate-instruction (syms)
  (case (car syms)
    (cpy (generate-copy (cdr syms)))
    (inc (generate-inc (cdr syms)))
    (dec (generate-dec (cdr syms)))
    (jnz (generate-jnz (cdr syms)))
    (otherwise (error (format nil "unknown instruction name ~a" (car syms))))))

(defun read-syms (line)
  (let (*read-eval*)
    (with-input-from-string (s line)
      (loop for sym = (read s nil nil)
         while sym collect sym))))

(defun read-instructions ()
  (with-open-file (f "day12-input")
    (when f
      (map 'vector #'identity
           (loop for line = (read-line f nil nil)
              while line 
              for syms = (read-syms line)
              collect (eval (generate-instruction syms)))))))

(defun interpret-program (instructions)
  (loop while (< *instruction-pointer* (length instructions))
     for instruction = (aref instructions *instruction-pointer*)
     do (funcall instruction))
  (aref *registers* 0))

(defun answer1 ()
  (setf *instruction-pointer* 0)
  (fill *registers* 0)
  (interpret-program (read-instructions)))

(defun answer2 ()
  (setf *instruction-pointer* 0)
  (fill *registers* 0)
  (setf (aref *registers* 2) 1)
  (interpret-program (read-instructions)))
