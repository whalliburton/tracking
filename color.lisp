(in-package :tracking)

(defun valid-rgba-hex-color (str)
  (when-let (len (and (stringp str) (length str)))
    (and (or (= len 8) (= len 6) (= len 4) (= len 3))
         (iter (for x from 1 to (1- len))
               (unless (digit-char-p (char str x) 16)
                 (return nil))
               (finally (return str))))))

(defun rgba-from-hex (hex)
  (let ((length (length hex))
        alpha-chars)
   (if (valid-rgba-hex-color hex)
     (progn
       (when (or (= length 8) (= length 4))
         (setf alpha-chars (subseq hex (if (= length 8) 6 3))
               hex (subseq hex 0 (if (= length 8) 6 3))))
       (let* ((short (= (length hex) 3))
              (r (parse-integer
                  (or (and short (subseq hex 0 1)) (subseq hex 0 2)) :radix 16))
              (g (parse-integer
                  (or (and short (subseq hex 1 2)) (subseq hex 2 4)) :radix 16))
              (b (parse-integer
                  (or (and short (subseq hex 2 3)) (subseq hex 4 6)) :radix 16))
              (a (and alpha-chars (parse-integer alpha-chars :radix 16))))
         (if short
           (values (+ r (* 16 r)) (+ g (* 16 g)) (+ b (* 16 b)) (and a (+ a (* 16 a))))
           (values r g b a))))
     (error "Invalid hex color ~A." hex))))

(defparameter *rgb-txt-file* (tracking-file "data/rgb.txt"))

(defun rgb->web (r g b)
  (let ((long (format nil "#~2,'0x~2,'0x~2,'0x"  r g b)))
    (if (and (char= (aref long 1) (aref long 2))
             (char= (aref long 3) (aref long 4))
             (char= (aref long 5) (aref long 6)))
      (format nil "#~A~A~A" (aref long 1) (aref long 3) (aref long 5))
      long)))

(defun parse-rgb-txt ()
  (with-input-from-file (s *rgb-txt-file*)
    (iter (for r = (read s nil))
          (for g = (read s nil))
          (for b = (read s nil))
          (for name = (read-line s nil))
          (if (null r)
            (return colors)
            (collect (cons (string-trim '(#\Space #\Tab) name) (rgb->web r g b)) into colors)))))

(defparameter *colors* (parse-rgb-txt))


