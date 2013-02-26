(in-package :tracking)

(defun render-icons (stream)
  (with-html-output (stream)
    ;;  (iter (for size from 10 to 200)
    ;;        (htm
    ;;         (:tr (:td (:img :src (format nil "/images/v/~(~A~)/FFF/~A" "money" size))
    ;;                   (:img :src (format nil "/images/v/~(~A~)/FFF/~A" "glass" size)))))))
    (:table
     (iter (for row in (group *icon-index* 20))
           (htm
            (:tr
             (iter (for (icon index) in row)
                   (htm (:td (:img :src (format nil "/images/v/~(~A~)/~A/FFF" icon 42)
                                   :title icon))))))))))

(defun render-colors (stream)
  (with-html-output (stream)
    (:table
     :style "border-collapse:collapse;width:100%;"
     (iter (for (name . hex) in *colors*)
           (htm (:tr (:td (:div :style (format nil "height:30px;padding:20px;background-color:~A;"
                                               hex)
                                (:span :style "text-shadow: 1px 1px #000;" (esc name))))
                     (:td (:div :style (format nil "height:30px;padding:20px;background-color:~A;"
                                               hex)
                                (:span :style "text-shadow: 1px 1px #000;" (esc hex))))))))))
