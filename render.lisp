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
   (cond
     ((equal what "icons") (render-icons stream))
     ((equal what "colors") (render-colors stream))
     ((not (session-value 'calibrated)) (render-calibration-page stream))
     (t (render-main-page stream)))))

(defun render-calibration-page (stream)
  (with-html-output (stream)
    (:div :style "font-size:32pt;" "Canine Print Identification")
    (:div :style "padding:40px 0 0 50px;font-size:24pt;" "Screen Calibration")
    (:div :style "padding:20px 0 0 70px;font-size:14pt;" "Please calibrate your screen using the plus and minus buttons.")
    (:div :style "padding:50px 0 0 100px;"
          (:table
           (:tr (:td) (:td) (:td :align :center
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
                   (str "setUnselectable([\"b1\",\"b2\",\"b3\",\"b4\",\"b5\"]);")))))

(defun render-main-page (stream)
  (with-html-output (stream)
    (:div :style "font-size:32pt;" "Canine Print Identification")
    (:div :style "padding:40px 0 0 50px;font-size:24pt;" "Main Menu")
    (:div :style "padding:20px 0 0 70px;font-size:14pt;" "Please select from the following options.")
    (:div :style "padding:60px 0 0 120px;;"
          (:table
           (:tr (:td
                 (:div :class "buttonb" :onclick "request(\"recalibrate\");" "Recalibrate Screen")))))))