(in-package :tracking)

(defun start-session ()
  (setf *deck-id* (start-deck-session "demo" "demo"))
  (start-printer-session))

(defparameter *templates*
  '(("tag" (("name" :string))))
  ;; '(("day" (("start" :date)))
  ;;   ("starting" "event" "day" nil (("time" :date))))
  )

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

