(in-package :tracking)

(defun initialize ()
  (setf hunchentoot:*catch-errors-p* nil)
  (ensure-directories-exist (tracking-file "logs/"))
  (start-session)
  (build)
  (load-data)
  (start-tracking)
  (start-sail)
  (format t "Welcome to Tracking.~%"))

(defmethod hunchentoot:maybe-invoke-debugger ((condition usocket:timeout-error))
  (warn "Timeout error. ~S" condition))