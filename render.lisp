(in-package :tracking)

(defparameter *maps-language* "en")

(defmacro render-page (title &rest body)
  `(with-html-output-to-string (stream)
     (:html
      (:head
       (:title (str ,title))
       (:link :rel "stylesheet" :type "text/css" :href "/css/tracking.css")
       ;; (:script :src (format nil "https://maps.google.com/maps/api/js?key=~A&sensor=true&language=~A"
       ;;                       *google-api-key* *maps-language*)
       ;;          :type "text/javascript")
       (:script :src "/js/tracking.js" :type "text/javascript")
       (:script :src "/js/iscroll.js" :type "text/javascript")
       ;; (:script :src "http://maps.stamen.com/js/tile.stamen.js?v1.2.1" :type "text/javascript")
       )
     (:body :id "body"
            ;; (:div :class "settings"  :style "width:32px;height:32px;"
            ;;       :onmouseover "showSettings();"
            ;;       :onmouseout "hideSettings();"
            ;;       (:img :id "settings-icon"
            ;;             :style "visibility:hidden;" :src "/images/cog.png"
            ;;             :onclick "showSettingsDialog();"))
            ,@body))))

(defun render-front-page (what)
  (render-page
   "Tracking"
   (let ((page (session-value 'page)))
     (cond
       ((equal what "icons") (render-icons stream))
       ((equal what "colors") (render-colors stream))
       ((not (session-value 'calibrated)) (render-calibration-page stream))
       ((eq page :training-data) (render-training-data-page stream))
       ((eq page :training-set) (render-training-set-page stream))
       (t (render-main-page stream))))))

(defmacro define-page (name title help &body body)
  `(defun ,(symb 'render- name '-page) (stream)
     (with-html-output (stream)
       (:div :style "font-size:32pt;" "Canine Print Identification")
       (:div :style "padding:40px 0 0 50px;font-size:24pt;" ,title)
       ,@(when help `((:div :style "padding:20px 0 0 70px;font-size:14pt;" ,help)))
       (:div :style "padding:50px 0 0 100px;"
             ,@body))))

(define-page calibration "Screen Calibration"
    "Please calibrate your screen using the plus and minus buttons."
  (:table
   (:tr (:td) (:td)
        (:td :align :center
             (:div :id "b1" :class "button" :onclick "request(\"decrease-calibration-width\");"
                   "&#x2296")
             (:div :id "b2" :class "button" :onclick "request(\"increase-calibration-width\");"
                   "&#x2295")))
   (:tr (:td (:div :style "padding-top:10px;")))
   (:tr
    (:td (:table
          (:tr (:td (:div :id "b3" :class "button" :onclick "request(\"decrease-calibration-height\");"
                          "&#x2296")))
          (:tr (:td (:div :id "b4" :class "button" :onclick "request(\"increase-calibration-height\");"
                          "&#x2295")))))
    (:td (:div :style "padding-right:6px;"))
    (:td (:img :id "calibration" :src (format nil "/images/calibration.png?id=~A" (random-string)))))
   (:tr (:td (:div :style "padding-top:30px;")))
   (:tr (:td) (:td) (:td :align :right
                         (:div :id "b5" :class "buttonb"
                               :onclick "request(\"finish-calibration\");"
                               "Done"))))
  (:script :type "text/javascript"
           (str "setUnselectable([\"b1\",\"b2\",\"b3\",\"b4\",\"b5\"]);")))

(define-page main
    "Main Menu"
    "Please select from the following options."
  (:table
   (:tr (:td
         (:div :style "width:250px;text-align:center;"
               :class "buttonb" :onclick "request(\"show-training-data\");" "Show Training Data")))
   (:tr (:td (:div :style "padding-top:10px;")))
   ;; (:tr (:td
   ;;       (:div :style "width:250px;text-align:center;"
   ;;             :class "buttonb" :onclick "request(\"show-ideal-prints\");" "Show Ideal
   ;;                Prints")))
   ;; (:tr (:td (:div :style "padding-top:10px;")))
   (:tr (:td
         (:div :style "width:250px;text-align:center;"
               :class "buttonb" :onclick "request(\"recalibrate\");" "Recalibrate Screen")))))

(defun show-training-data ()
  (setf (session-value 'page) :training-data
        (session-value 'training-set) nil)
  "go(\"/\");")

(defun show-main-menu ()
  (setf (session-value 'page) nil)
  "go(\"/\");")

(defun rotate-rows (rows)
  (apply #'mapcar #'list rows))

(defun print-training-data-table (data stream &optional only-mean)
  (with-html-output (stream)
    (:table :class "training-data" :style "border-collapse:collapse;"
            (:tr :class "headings"
                 (iter (for name in *tracking-canine-centers-headers*)
                       (for x from 1)
                       (unless (= x 42)
                         (htm (:td (esc name))))))
            (unless only-mean
              (iter (with even)
                    (for row in data)
                    (htm
                     (:tr :onclick (format nil "selectTrainingSet(~A);" (car row))
                          :class (if even "odd" "even")
                          (iter (for val in (cdr row))
                                (for x from 1)
                                (unless (= x 42)
                                  (htm (:td (esc val)))))))
                    (setf even (not even))))
            (iter (for heading in '("mean" "median" "sd" "variance") )
                  (for fn in (list #'stats:mean #'stats:median #'stats:sd #'stats:variance))
                  (htm (:tr :class "footings"
                        (iter (for column in (cdr (rotate-rows data)))
                              (for index from 0)
                              (cond
                                ((= index 0) (htm (:td (esc heading))))
                                ((= index 41))
                                ((> index 10) (htm (:td (fmt
                                                         "~,2F"
                                                         (funcall fn (mapcar #'parse-float
                                                                             (remove-if (lambda (el) (zerop (length el))) column))))) ))
                                (t (htm (:td)))))))))))

(define-page training-data
    "Training Data"
    nil
  (let ((tab (session-value 'tab)))
    (htm
     (:div :style "width:300px;text-align:center;"
           :class "buttonb" :onclick "request(\"show-main-menu\");" "Go back to the main menu")
     (:div :style "padding-top:40px;")
     (:table (:tr
              (:td (:div :style "width:180px;text-align:center;"
                         :class (if (eq tab nil) "buttond" "buttonc")
                         :onclick "request(\"show-tab-all-together\");" "All Together"))
              (:td (:div :style "width:180px;text-align:center;"
                         :class (if (eq tab :by-species) "buttond" "buttonc")
                         :onclick "request(\"show-tab-by-species\");" "By Species"))
              (:td (:div :style "width:250px;text-align:center;"
                         :class (if (eq tab :by-species-only-mean) "buttond" "buttonc")
                         :onclick "request(\"show-tab-by-species-only-mean\");"
                         "By Species Only Mean"))))
     (cond
       ((eq tab nil)
        (htm (:div :style "padding-top:40px;"))
        (print-training-data-table *tracking-canine-centers* stream))
       ((or (eq tab :by-species) (eq tab :by-species-only-mean))
        (iter (for species in '("fox" "coyote" "wolf" "dog"))
              (for rows in *tracking-canine-centers-by-species*)
              (htm
               (:div :style "padding-top:40px;")
               (:div :style "font-size:24pt;" (esc species))
               (:div :style "padding-top:10px;")
               (print-training-data-table rows stream (eq tab :by-species-only-mean)))))))))

(defun show-tab-all-together ()
  (setf (session-value 'tab) nil)
  "go(\"/\");")

(defun show-tab-by-species ()
  (setf (session-value 'tab) :by-species)
  "go(\"/\");")

(defun show-tab-by-species-only-mean ()
  (setf (session-value 'tab) :by-species-only-mean)
  "go(\"/\");")

(defun select-training-set (index)
  (setf (session-value 'training-set) (parse-integer index)
        (session-value 'page) :training-set)
  "go(\"/\");")

(define-page training-set
    "Training Set"
    nil
  (let ((index (session-value 'training-set)))
    (htm
     (:div :style "width:375px;text-align:center;"
           :class "buttonb" :onclick "request(\"show-training-data\");" "Go back to the training data table")
     (:div :style "padding-top:40px;")
     (:table :class "training-set" :style "border-collapse:collapse;"
      (iter
       (for heading in *tracking-canine-centers-headers*)
       (for val in (cdr (nth index *tracking-canine-centers*)))
       (for x from 1)
       (unless (= x 42)
         (htm
          (:tr (:td (esc heading)) (:td (esc val))))))))))

