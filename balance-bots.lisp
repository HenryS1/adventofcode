(defclass bot ()
  ((id :accessor id :initarg :id)
   (inputs :accessor inputs :initform nil)
   (outputs :accessor outputs :initarg :outputs :initform nil)))

(defmethod add-bot ((bot bot) (bot-graph bot-graph))
  (with-slots (graph) bot-graph
    (setf (gethash (id bot) graph) bot)))

(defclass bot-graph ()
  ((graph :accessor graph :initform (make-hash-table))))
