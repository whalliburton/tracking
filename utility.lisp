(in-package :tracking)

(defun asdf-base-path (name)
  (directory-namestring (asdf:component-pathname (asdf:find-system name))))

(defun tracking-file (base)
  (concatenate 'string (asdf-base-path :tracking) base))

(defun slurp-file (filename &optional external-format)
  (with-input-from-file (stream filename :external-format (or external-format :utf-8))
    (let ((seq (make-string (file-length stream))))
      (read-sequence seq stream)
      seq)))

(defun slurp-stream (stream)
  "Slurp all octets from STREAM. Returns a vector of octets."
  (let ((seq (make-array (file-length stream) :element-type '(unsigned-byte 8))))
    (read-sequence seq stream)
    seq))

(defun slurp-as-octets (filename)
  "Slurp the contents of the file designated by FILENAME, returning
a vector of octets."
  (with-input-from-file (f filename :element-type '(unsigned-byte 8))
    (slurp-stream f)))

(defun slurp-lines (filename &optional external-format)
  (with-open-file (f filename :direction :input :if-does-not-exist :error
                     :external-format (or external-format :utf8))
    (iter (for line = (read-line f nil))
          (while line)
          (collect line))))

(defun last1 (list)
  (car (last list)))

(defun ensure-list (list)
  "If LIST is a list, it is returned. Otherwise returns the list designated by LIST."
  (if (listp list)
      list
      (list list)))

(defun group (source n)
  (when (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                 (rec rest (cons
                            (subseq source 0 n)
                            acc))
                 (nreverse
                  (cons source acc))))))
    (when source (rec source nil))))

(defun safe-parse-integer (string)
  (let ((digit (position-if (lambda (el) (digit-char-p el)) string)))
    (cond
      ((zerop digit) (parse-integer string :junk-allowed t))
      (digit (parse-integer (subseq string digit) :junk-allowed t)))))


(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(defun view-png (data)
  (let ((filename (helpers:temporary-file-name)))
    (unwind-protect
         (progn
           (with-output-to-file (stream filename :element-type '(unsigned-byte 8))
             (write-sequence data stream))
           (sb-ext:run-program "/usr/bin/feh" (list filename)))
      (sb-posix:unlink filename))))
