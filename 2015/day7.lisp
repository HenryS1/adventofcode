(defun read-syms (line)
  (let (*read-eval*)
    (with-input-from-string (s line)
      (loop for sym = (read s nil nil)
         while sym collect sym))))

(defun namespace (sym)
  (intern (concatenate 'string "FUN-" (symbol-name sym))))

(defun preprocess (sym)
  (if (symbolp sym)
      `(,(namespace sym))
      sym))

(defun build-command (line)
  (let ((syms (read-syms line)))
    (labels ((fun-template (body)
                 `(let (val)
                    (defun ,(namespace (car (last syms))) ()
                        (or val
                         (setf val 
                               ,body))))))
            (cond ((find 'not syms) 
                   (fun-template `(lognot ,(preprocess (cadr syms)))))
                  ((find 'and syms)
                   (fun-template `(logand ,(preprocess (car syms))
                                          ,(preprocess (caddr syms)))))
                  ((find 'or syms)
                   (fun-template `(logior ,(preprocess (car syms))
                                                  ,(preprocess (caddr syms)))))
                  ((find 'lshift syms)
                   (fun-template `(ash ,(preprocess (car syms))
                                       ,(preprocess (caddr syms)))))
                  ((find 'rshift syms)
                   (fun-template `(ash ,(preprocess (car syms))
                                       (- ,(preprocess (caddr syms))))))
                  (t (fun-template (preprocess (car syms))))))))

(defun read-commands ()
  (with-open-file (f "input7")
    (when f
      (loop for line = (read-line f nil nil)
         while line 
         collect (build-command line)))))

(defun part-one ()
  (let ((sb-ext:*muffled-warnings* 'sb-kernel:redefinition-warning)
        (commands (read-commands)))
    (loop for command in commands
       do (compile (eval command)))
    (loop for command in commands 
       do (compile (eval command)))
    (funcall #'fun-a)))

(defun part-two ()
  (let ((sb-ext:*muffled-warnings* 'sb-kernel:redefinition-warning)
        (commands (read-commands))
        (a (part-one)))
    (loop for command in commands
       do (compile (eval command)))
    (loop for command in commands
       do (compile (eval command)))
    (setf (symbol-function 'fun-b)
          (lambda () a))
    (funcall #'fun-a)))
