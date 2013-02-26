(in-package :tracking)

(defun draw-icon (icon size fill-color stroke-color)
  (with-canvas (:width (+ size (floor (* size (/ 1 10)))) :height (+ size (floor (* size (/ 1 10)))))
    (let ((font (get-font (tracking-file "fontawesome-webfont.ttf"))))
      ;; (set-rgba-stroke 1.0 0 0 1.0)
      ;; (rectangle 0 0 1 1)
      ;; (rectangle (1- size) (1- size) 1 1)
      (set-font font size)
      (if fill-color
        (multiple-value-bind (r g b a) (rgba-from-hex fill-color)
          (set-rgba-fill (float (/ r 255))
                         (float (/ g 255))
                         (float (/ b 255))
                         (float (/ (or a 255) 255))))
        (set-rgba-fill 1.0 1.0 1.0 1.0))
      (when stroke-color
        (multiple-value-bind (r g b a) (rgba-from-hex stroke-color)
          (set-rgba-stroke (float (/ r 255))
                           (float (/ g 255))
                           (float (/ b 255))
                           (float (/ (or a 255) 255))))        )
      (set-line-width 1.0)
      (string-paths 0 (floor (* size (/ 2 10)))
                   (princ-to-string (icon-index-from-name icon)))
      (if stroke-color
        (fill-and-stroke)
        (fill-path))
      (flexi-streams:with-output-to-sequence (stream)
        (save-png-stream stream)))))

(defun-simple-memoized draw-icon-memoized (args :test equal)
  (apply #'draw-icon args))

(defparameter *debug-vector* nil)

(defun vector-dispatch ()
  (setf (hunchentoot:content-type*) "image/png")
  (let ((hilight (hunchentoot:get-parameter "hilight")))
    (let ((raw (subseq (hunchentoot:script-name*) 10)))
      (destructuring-bind (name &optional size fill stroke) (split-sequence #\/ raw)
        (if *debug-vector*
          (draw-icon name (or (and size (parse-integer size)) 24 ) fill stroke)
          (draw-icon-memoized (list name
                                    (if hilight
                                      48
                                      (or (and size (parse-integer size)) 24))
                                    fill stroke)))))))

