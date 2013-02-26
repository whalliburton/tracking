(in-package :tracking)

(helpers:add-config-parameters
 deck-local-socket-name             "/mnt/projects/sockets/deck"
 deck-cache-invalidation-port       2003
 deck-uses-sharder                  nil)

(defun initialize ()
  (setf hunchentoot:*catch-errors-p* nil)
  (ensure-directories-exist (tracking-file "logs/"))
  (start-session)
  (build)
  (start-tracking))

(defmethod hunchentoot:maybe-invoke-debugger ((condition usocket:timeout-error))
  (warn "Timeout error. ~S" condition))