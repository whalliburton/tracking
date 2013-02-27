(in-package :tracking)

(defparameter *centers-headers* nil)
(defparameter *centers* nil)
(defparameter *centers-by-species* nil)

(defun row-species (row)
  (nth 5 row))

(defun row-center-data (row)
  (let ((raw (copy-list (nthcdr 39 row))))
    (setf (cdddr raw) (cddddr raw))
    raw))

(defun split-by-species ()
  (iter (for row in *centers*)
        (let ((species (row-species row)))
          (cond
            ((string-equal species "f") (collect row into foxes))
            ((string-equal species "c") (collect row into coyotes))
            ((string-equal species "w") (collect row into wolves))
            (t (collect row into dogs))))
        (finally (return (list foxes coyotes wolves dogs)))))

(defun load-data ()
  (let ((raw (iter (for line in (slurp-lines (tracking-file "data/canine-centers.txt")))
                   (let ((columns
                           (split-sequence #\tab (string-right-trim '(#\return) line))))
                     (when (= (length columns) 47)
                       (collect columns))))))
    (setf *centers-headers*
          (iter (for a in (first raw))
                (for b in (second raw))
                (collect (if (equal a b) a (concatenate 'string a " " b))))
          *centers*
          (iter (for row in (cddr raw))
                (for index from 0)
                (collect (cons index row)))
          *centers-by-species*
          (split-by-species))))
