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
       ((eq page :enter-new-print) (render-enter-new-print-page stream))
       ((eq page :view-print) (render-view-print-page stream))
       ((eq page :view-all-prints) (render-view-all-prints-page stream))
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
               :class "buttonb" :onclick "request(\"show-all-prints\");" "View All Prints")))
   (:tr (:td (:div :style "padding-top:10px;")))
   (:tr (:td
         (:div :style "width:250px;text-align:center;"
               :class "buttonb" :onclick "request(\"enter-new-print\");" "Enter a New Print")))
   (:tr (:td (:div :style "padding-top:10px;")))
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

(defun print-training-data-table (data stream &optional only-mean only-centers)
  (with-html-output (stream)
    (:table :class "training-data" :style "border-collapse:collapse;"
            (:tr :class "headings"
                 (iter (for name in *centers-headers*)
                       (for x from 1)
                       (if (and only-mean (= x 1))
                         (htm (:td))
                         (unless (or (= x 42)
                                     (and only-mean (> x 1) (< x 39))
                                     (and only-centers (> x 11) (< x 39)))
                           (htm (:td (esc name)))))))
            (unless only-mean
              (iter (with even)
                    (for row in data)
                    (htm
                     (:tr :onclick (format nil "selectTrainingSet(~A);" (car row))
                          :class (if even "odd" "even")
                          (iter (for val in (cdr row))
                                (for x from 1)
                                (unless (or (= x 42)
                                            (and only-mean (> x 1) (< x 39))
                                            (and only-centers (> x 11) (< x 39)))
                                  (htm (:td (esc val)))))))
                    (setf even (not even))))
            (iter (for row in (calculate-statistics data only-mean only-centers))
                  (htm (:tr :class "footings"
                            (iter (for column in row)
                                  (if (numberp column)
                                    (htm (:td (fmt "~,1F" column)))
                                    (htm (:td (esc column)))))))))))

(define-page training-data
    "Training Data"
    nil
  (let ((tab (session-value 'tab))
        (only-centers (session-value 'only-centers)))
    (htm
     (:div :style "width:300px;text-align:center;"
           :class "buttonb" :onclick "request(\"show-main-menu\");" "Go back to the main menu")
     (:div :style "padding-top:40px;")
     (:table (:tr
              (:td (:div :style "width:200px;text-align:center;"
                         :class (if (eq tab nil) "buttond" "buttonc")
                         :onclick "request(\"show-tab-all-together\");" "All Together"))
              (:td (:div :style "width:200px;text-align:center;"
                         :class (if (eq tab :by-species) "buttond" "buttonc")
                         :onclick "request(\"show-tab-by-species\");" "By Species"))
              (:td (:div :style "width:250px;text-align:center;"
                         :class (if (eq tab :by-species-only-mean) "buttond" "buttonc")
                         :onclick "request(\"show-tab-by-species-only-mean\");"
                         "By Species Only Mean"))))
     (:table (:tr
              (:td (:div :style "width:200px;text-align:center;"
                         :class (if (not only-centers) "buttond" "buttonc")
                         :onclick "request(\"show-all-data\");" "All Data"))
              (:td (:div :style "width:200px;text-align:center;"
                         :class (if only-centers "buttond" "buttonc")
                         :onclick "request(\"show-center-data\");" "Only Center Data"))))
     (cond
       ((eq tab nil)
        (htm (:div :style "padding-top:40px;"))
        (print-training-data-table *centers* stream nil only-centers))
       ((or (eq tab :by-species) (eq tab :by-species-only-mean))
        (iter (for species in '("fox" "coyote" "wolf" "dog"))
              (for rows in *centers-by-species*)
              (htm
               (:div :style "padding-top:40px;")
               (:div :style "font-size:24pt;" (esc species))
               (:div :style "padding-top:10px;")
               (print-training-data-table rows stream (eq tab :by-species-only-mean) only-centers))))))))

(defun show-all-data ()
  (unless  (eq (session-value 'tab) :by-species-only-mean)
    (setf (session-value 'only-centers) nil)
    "go(\"/\");"))

(defun show-center-data ()
  (setf (session-value 'only-centers) t)
  "go(\"/\");")

(defun show-tab-all-together ()
  (setf (session-value 'tab) nil)
  "go(\"/\");")

(defun show-tab-by-species ()
  (setf (session-value 'tab) :by-species)
  "go(\"/\");")

(defun show-tab-by-species-only-mean ()
  (setf (session-value 'tab) :by-species-only-mean
        (session-value 'only-centers) t)
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
       (for heading in *centers-headers*)
       (for val in (cdr (nth index *centers*)))
       (for x from 1)
       (unless (= x 42)
         (htm
          (:tr (:td (esc heading)) (:td (esc val))))))))))

(defun enter-new-print ()
  (setf (session-value 'page) :enter-new-print)
  "go(\"/\");")

(define-page enter-new-print "Enter a New Print"
    "Please enter the measurements for the new print. It is OK to leave fields blank."
  (:table :class "new-print"
   (:tr (:td  :rowspan 5 :style "virtical-align:middle;" "centers"))
   (:tr (:td) (:td "toe 2") (:td (:input :type "text" :id "t2")) (:td "mm"))
   (:tr (:td) (:td "toe 3") (:td (:input :type "text" :id "t3")) (:td "mm"))
   (:tr (:td) (:td "toe 4") (:td (:input :type "text" :id "t4")) (:td "mm"))
   (:tr (:td) (:td "toe 5") (:td (:input :type "text" :id "t5")) (:td "mm"))
   (:tr (:td :rowspan 5 :style "virtical-align:middle;" "splays"))
   (:tr (:td) (:td "1") (:td (:input :type "text" :id "s1")) (:td "mm"))
   (:tr (:td) (:td "2") (:td (:input :type "text" :id "s2")) (:td "mm"))
   (:tr (:td) (:td "3") (:td (:input :type "text" :id "s3")) (:td "mm"))
   (:tr (:td) (:td "4") (:td (:input :type "text" :id "s4")) (:td "mm"))
   (:tr (:td :style "padding-top:20px;"))
   (:tr (:td) (:td) (:td) (:td :align :right
                               (:div :id "b5" :class "buttonb"
                                     :onclick "sendNewPrint();"
                                     "Done")
                               (:span :style "padding-right:10px;")
                               (:div :id "b5" :class "buttonb"
                               :onclick "request(\"show-all-prints\");" "Cancel")))

   (:script :type "text/javascript" (str "focus(\"t2\");"))))

(defun show-error-dialog (text)
  (format nil "showDialog(~S);"
          (with-html-output-to-string (stream)
            (:div :style "background-color:black;border:1px solid red;padding:10px;"
                  (:div :style "font-size:24px;" (esc text))
                  (:div :style "height:20px;")
                  (:div :class "button"
                        :onclick "closeDialog();" (esc "Ok"))))))

(defun send-new-print (data)
  (let ((data
          (handler-case
              (iter (for el in (split-sequence #\, (url-decode data)))
                    (collect (when (plusp (length el)) (parse-float el))))
            (error ()
              (return-from send-new-print (show-error-dialog "Error in data."))))))
    (destructuring-bind (t2 t3 t4 t5 s1 s2 s3 s4) data
      (let ((id (deck:add-node "tracking:print" `(("t2" ,t2)
                                                  ("t3" ,t3)
                                                  ("t4" ,t4)
                                                  ("t5" ,t5)
                                                  ("s1" ,s1)
                                                  ("s2" ,s2)
                                                  ("s3" ,s3)
                                                  ("s4" ,s4)
                                                  ("fs" ,(calculate-score 0 data))
                                                  ("cs" ,(calculate-score 1 data))
                                                  ("ws" ,(calculate-score 2 data))
                                                  ("ds" ,(calculate-score 3 data))))))
        (setf (session-value 'page) :view-print
              (session-value 'print-id) id))))
  "go(\"/\");")

;; Euclidian distance from the means
(defun calculate-score (species data)
  (let ((means (nth species *species-means*)))
    (let ((base
            (iter (for el in data)
                  (for mean in means)
                  (when el
                    (summing (* (- el mean) (- el mean)))))))
      (if base
        (sqrt base)
        0))))

(define-page view-print
    "View Print"
    nil
  (:div :style "width:300px;text-align:center;"
        :class "buttonb" :onclick "request(\"show-all-prints\");" "Go back to the print table")
  (:div :style "padding-top:40px;")
  (:div :style "width:300px;text-align:center;"
        :class "buttonb" :onclick (format nil "deletePrint(~A);" (session-value 'print-id))
        "Delete this print")
  (:div :style "padding-top:40px;")
  (let* ((id (session-value 'print-id))
         (node (deck:get-node id)))
    (with-field-values (t2 t3 t4 t5 s1 s2 s3 s4 fs cs ws ds) node
      (htm (:table :class "view-print"
                   (:tr (:td  :rowspan 5 :style "virtical-align:middle;" "centers"))
                   (:tr (:td) (:td "toe 2") (:td (fmt "~,1F" t2)) (:td "mm"))
                   (:tr (:td) (:td "toe 3") (:td (fmt "~,1F" t3)) (:td "mm"))
                   (:tr (:td) (:td "toe 4") (:td (fmt "~,1F" t4)) (:td "mm"))
                   (:tr (:td) (:td "toe 5") (:td (fmt "~,1F" t5)) (:td "mm"))
                   (:tr (:td :rowspan 5 :style "virtical-align:middle;" "splays"))
                   (:tr (:td) (:td "1") (:td (fmt "~,1F" s1)) (:td "mm"))
                   (:tr (:td) (:td "2") (:td (fmt "~,1F" s2)) (:td "mm"))
                   (:tr (:td) (:td "3") (:td (fmt "~,1F" s3)) (:td "mm"))
                   (:tr (:td) (:td "4") (:td (fmt "~,1F" s4)) (:td "mm"))
                   (:tr (:td :rowspan 5 :style "virtical-align:middle;" "scores"))
                   (:tr (:td) (:td "fox") (:td (fmt "~,1F" fs)))
                   (:tr (:td) (:td "coyote") (:td (fmt "~,1F" cs)))
                   (:tr (:td) (:td "wolf") (:td (fmt "~,1F" ws)))
                   (:tr (:td) (:td "dog") (:td (fmt "~,1F" ds)))
                   (:tr (:td :style "padding-top:20px;")))))))

(defparameter +time-format+
  '(:month #\/(:day 2) #\/ (:year 4)))

(define-page view-all-prints
    "View All Prints"
    nil
  (:div :style "width:300px;text-align:center;"
        :class "buttonb" :onclick "request(\"show-main-menu\");" "Go back to the main menu")
  (:div :style "padding-top:20px;")
  (:div :style "width:300px;text-align:center;"
        :class "buttonb" :onclick "request(\"enter-new-print\");" "Enter a New Print")
  (:div :style "padding-top:30px;")
  (let ((prints (deck:search "demo:print")))
    (if prints
      (htm
       (:table :class "all-prints"
               (:tr :class "headings" (:td)
                    (:td :style "text-align:center;" :colspan 4 "centers")
                    (:td :style "text-align:center;" :colspan 4 "splays" )
                    (:td :style "text-align:center;" :colspan 4 "scores" ))
               (:tr :class "headings"
                    (:td "created")
                    (:td "toe 2") (:td "toe 3") (:td "toe 4") (:td "toe 5")
                    (:td "1") (:td "2") (:td "3") (:td "4")
                    (:td "fox") (:td "coyote") (:td "wolf") (:td "dog"))
               (iter
                (with even)
                (for print in prints)
                (with-field-values (t2 t3 t4 t5 s1 s2 s3 s4 fs cs ws ds) print
                  (htm (:tr
                        :style "cursor:pointer"
                        :onclick (format nil "selectPrint(~A);" (id print))
                        :class (if even "odd" "even")
                        (:td (esc (format-timestring nil (inception print) :format +time-format+)))
                        (:td (str (if t2 (format nil "~,1F" t2) "")))
                        (:td (str (if t3 (format nil "~,1F" t3) "")))
                        (:td (str (if t4 (format nil "~,1F" t4) "")))
                        (:td (str (if t5 (format nil "~,1F" t5) "")))
                        (:td (str (if s1 (format nil "~,1F" s1) "")))
                        (:td (str (if s2 (format nil "~,1F" s2) "")))
                        (:td (str (if s3 (format nil "~,1F" s3) "")))
                        (:td (str (if s4 (format nil "~,1F" s4) "")))
                        (:td (str (if fs (format nil "~,1F" fs) "")))
                        (:td (str (if fs (format nil "~,1F" cs) "")))
                        (:td (str (if fs (format nil "~,1F" ws) "")))
                        (:td (str (if fs (format nil "~,1F" ds) ""))))))
                (setf even (not even)))))
      (htm (:div :style "font-size:24px;" "There are no prints.")))))

(defun show-all-prints ()
  (setf (session-value 'page) :view-all-prints)
  "go(\"/\");")

(defun select-print (id)
  (let ((id (parse-integer id)))
    (setf (session-value 'page) :view-print
          (session-value 'print-id) id)
    "go(\"/\");"))

(defun delete-print (id)
  (let ((id (parse-integer id)))
    (deck:delete-node id)
    (setf (session-value 'page) :view-all-prints)
    "go(\"/\");"))
