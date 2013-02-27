(in-package :tracking)

(defvar *tracking-acceptor* nil)
(defvar *tracking-port* 1235)
(defvar *tracking-username* "fast")
(defvar *tracking-password* "track")

(defun start-tracking ()
  (when *tracking-acceptor* (hunchentoot:stop *tracking-acceptor*))
  (setf hunchentoot:*message-log-pathname* (tracking-file (format nil "logs/message-~A.log" (now)))
        hunchentoot:*access-log-pathname* (tracking-file (format nil "logs/access-~A.log" (now)))
        *tracking-acceptor*
        (hunchentoot:start
         (make-instance 'hunchentoot:acceptor :port *tracking-port*))
        hunchentoot:*dispatch-table*
        `(,(hunchentoot:create-prefix-dispatcher "/css/tracking.css" 'tracking-css)
          ,(hunchentoot:create-prefix-dispatcher "/a" 'ajax-handler)
          ,(hunchentoot:create-static-file-dispatcher-and-handler "/futura.ttf"
                                                                  (tracking-file "futura.ttf"))
          ,(hunchentoot:create-static-file-dispatcher-and-handler "/fontawesome-webfont.ttf"
                                                                  (tracking-file "fontawesome-webfont.ttf"))
          ,(hunchentoot:create-static-file-dispatcher-and-handler
            "/js/iscroll.js" (tracking-file "data/iscroll.js"))
          ,(hunchentoot:create-prefix-dispatcher "/js/tracking.js" 'tracking-js:js-file)
          ,(hunchentoot:create-prefix-dispatcher "/images/v/" 'vector-dispatch)
          ,(hunchentoot:create-prefix-dispatcher "/images/calibration.png" 'calibration-image-dispatch)
          ,(hunchentoot:create-folder-dispatcher-and-handler "/images/" (tracking-file "images/"))
          ,(hunchentoot:create-prefix-dispatcher "/favicon.ico" 'favicon-dispatch)
          hunchentoot:dispatch-easy-handlers
          hunchentoot:default-dispatcher)))

(defun tracking-css () (slurp-file (tracking-file "tracking.css")))

(defun favicon-dispatch () (slurp-as-octets (tracking-file "icon.ico")))

(hunchentoot:define-easy-handler (front-page :uri "/") (what)
  (multiple-value-bind (username password) (hunchentoot:authorization)
    (if (not (and (string= username *tracking-username*)
                  (string= password *tracking-password*)))
      (hunchentoot:require-authorization "tracking")
      (progn
        (hunchentoot:start-session)
        (render-front-page what)))))

(defun ajax-handler ()
  (let ((args (mapcar (lambda (el) (let ((pos (position #\= el)))
                                     (if pos
                                       (list
                                        (intern (string-upcase (subseq el 0 pos)) :keyword)
                                        (subseq el (1+ pos)))
                                       el)))
                      (split-sequence
                       #\&
                       (subseq (hunchentoot:request-uri hunchentoot:*request*)
                               (1+ (position #\& (hunchentoot:request-uri hunchentoot:*request*))))))))
    (flet ((with-args (&rest names)
             (loop for name in names
                   collect (or (second (assoc name args))
                               (error "Missing arg ~S." name)))))
      (let ((command (first (with-args :command))))
        (format t "command: ~A  ~S~%" command args)
        (cond
          ((string-equal command "increase-calibration-width")
           (increase-calibration-width))
          ((string-equal command "decrease-calibration-width")
           (decrease-calibration-width))
          ((string-equal command "increase-calibration-height")
           (increase-calibration-height))
          ((string-equal command "decrease-calibration-height")
           (decrease-calibration-height))
          ((string-equal command "finish-calibration")
           (finish-calibration))
          ((string-equal command "recalibrate")
           (recalibrate))
          ((string-equal command "show-training-data")
           (show-training-data))
          ((string-equal command "show-main-menu")
           (show-main-menu))
          ((string-equal command "select-training-set")
           (apply #'select-training-set (with-args :index)))
          ((string-equal command "show-tab-all-together")
           (show-tab-all-together))
          ((string-equal command "show-tab-by-species")
           (show-tab-by-species))
          ((string-equal command "show-tab-by-species-only-mean")
           (show-tab-by-species-only-mean)))))))

(defmacro with-sessions ((session-id-var session-var) &body body)
  `(iter (for (,session-id-var . ,session-var) in (with-mutex ((session-db-lock *tracking-acceptor*))
                             (session-db *tracking-acceptor*)))
         ,@body))

(defun list-all-sessions ()
  (with-sessions (nil session) (collect session)))

(defun session-seconds-since-last-click (session)
  (timestamp-difference
   (now)
   (universal-to-timestamp (hunchentoot::session-last-click session))))

(defun session-duration (session)
  (timestamp-difference
   (now)
   (universal-to-timestamp (hunchentoot::session-start session))))

(defun short-user-agent-string (session)
  (let ((user-agent (hunchentoot:session-user-agent session)))
    (cond
      ((scan "Firefox" user-agent) "firefox")
      ((scan "Conkeror" user-agent) "conkeror")
      ((scan "Chrome" user-agent) "chrome")
      ((scan "Drakma" user-agent) "drakma")
      ((scan "MSIE 9" user-agent) "ie9")
      (t user-agent))))

(defun list-sessions (&optional detail)
  (if-let (sessions (list-all-sessions))
    (print-table
     (iter (for session in (sort sessions #'< :key #'session-seconds-since-last-click))
           (let ((id (hunchentoot::session-id session)))
             (collect
                 (nconc
                  (list
                   id
                   (seconds-to-duration-string (session-seconds-since-last-click session) 0)
                   (seconds-to-duration-string (session-duration session) 0)
                   (short-user-agent-string session)
                   (session-remote-addr session))))
             (when detail
               (let ((start (universal-to-timestamp
                             (hunchentoot::session-start session)))
                     (last-click (universal-to-timestamp
                                  (hunchentoot::session-last-click session))))
                 (collect `(:subtable
                            ("user agent" ,(session-user-agent session))
                            ("remote addr" ,(session-remote-addr session))
                            ("start" ,(format nil "~A  ~A"
                                              start
                                              (seconds-to-duration-string
                                               (round (timestamp-difference (now) start)) 0)))
                            ("last click" ,(format nil "~A  ~A"
                                                   last-click
                                                   (seconds-to-duration-string
                                                    (round (timestamp-difference (now) last-click))
                                                    0)))
                            ("")))))))
     :headings '("id" "idle time" "duration" "agent" "ip"))
    (format t "no sessions~%")))

(defalias w list-sessions)

(defun reset-sessions ()
  (let ((hunchentoot:*acceptor* *tracking-acceptor*))
    (hunchentoot:reset-sessions)))