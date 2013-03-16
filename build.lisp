(in-package :tracking)

(defun start-session ()
  (setf *deck-id* (start-deck-session "tracking" "07D513B21ADDCB4CE" :possibly-create-user t))
  (start-printer-session))

(defparameter *templates*
  '(("print" (("name" :string)
              ("t2" :float :validation (:allow-nil t))
              ("t3" :float :validation (:allow-nil t))
              ("t4" :float :validation (:allow-nil t))
              ("t5" :float :validation (:allow-nil t))
              ("s1" :float :validation (:allow-nil t))
              ("s2" :float :validation (:allow-nil t))
              ("s3" :float :validation (:allow-nil t))
              ("s4" :float :validation (:allow-nil t))
              ("fs" :float :validation (:allow-nil t))
              ("cs" :float :validation (:allow-nil t))
              ("ws" :float :validation (:allow-nil t))
              ("ds" :float :validation (:allow-nil t))))))

(defun create-templates ()
  (when *templates*
    (iter (for name in (mapcar #'car *templates*)) (ignore-errors (deck:delete-template name)))
    (iter (for template in *templates*)
          (if (aand (second template) (listp it) (listp (car it)))
            (deck:add-node-template (first template) (second template))
            (deck:add-edge-template (first template) (second template) (third template)
                                    :reverse-name (fourth template)
                                    :fields (fifth template))))))

(defun build ()
  (create-templates))

(defmethod sail:serialize-replacement ((fields-base fields-base))
  (id fields-base))

