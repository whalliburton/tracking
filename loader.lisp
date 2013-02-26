(in-package :tracking)

(defparameter *tracking-canine-centers* nil)

(defun load-data ()
  (setf *tracking-canine-centers*
        (iter (for line in (slurp-lines (tracking-file "data/canine-centers.txt")))
              (let ((columns
                      (split-sequence #\tab (string-right-trim '(#\return) line))))
                (when (= (length columns) 47)
                  (collect columns))))))

(defun canine-centers-column-names ()
  (iter (for a in (first *tracking-canine-centers*))
        (for b in (second *tracking-canine-centers*))
        (collect (if (equal a b) a (concatenate 'string a " " b)))))

(defun print-canine-centers-column-names ()
  (iter (for name in (canine-centers-column-names))
        (for index from 1)
        (format t "~A ~A~%" index name)))

