(in-package :tracking)

(helpers:add-config-parameters
 deck-local-socket-name             "/mnt/projects/sockets/deck"
 deck-cache-invalidation-port       2003
 deck-uses-sharder                  nil
 tracking-rpc-port                      12412
 tracking-host                          "ec2-local-ip")

(defun start-sail ()
  (sail:start-sail-server
   "tracking"
   :port (helpers:config tracking-rpc-port nil)
   :host (helpers:host-or-local-ip tracking-host)
   :public '()
   :private ()))

(defun restart-sail ()
  (when *sails* (stop-sails))
  (sleep 0.25) ;; to get back the port
  (start-sail))
