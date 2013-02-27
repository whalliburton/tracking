(in-package :tracking)

(defparameter *centers-headers* nil)
(defparameter *centers* nil)
(defparameter *centers-by-species* nil)
(defparameter *species-means* nil)

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

(defun calculate-statistics (rows &optional only-mean only-centers)
  (iter (for heading in '("mean" "median" "sd" "variance") )
        (for fn in (list #'stats:mean #'stats:median #'stats:sd #'stats:variance))
        (collect
            (iter (for column in (cdr (rotate-rows rows)))
                  (for x from 1)
                  (cond
                    ((= x 1) (collect heading))
                    ((or (= x 42)
                         (and only-mean (> x 1) (< x 39))
                         (and only-centers (> x 11) (< x 39))))
                    ((> x 11) (collect
                                (funcall fn (mapcar #'parse-float
                                                    (remove-if (lambda (el) (zerop (length el)))
                                                               column)))))
                    (t (collect nil)))))))

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
