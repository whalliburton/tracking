(in-package :tracking-js)

(defpsmacro with-id ((var id) &body body)
  `(let ((,var (get-by-id ,id)))
     (if ,var
       (progn
         ,@body))))

(defpsmacro plusp (el)
  `(> ,el 0))

(defpsmacro console (&rest rest)
  `(cond
     ((=== *browser* :firefox) ((@ console log) ,@rest))
     ((or (=== *browser* :chrome) (=== *browser* :trident))
      ((@ console log)
       (+ ,@(butlast (loop for arg in rest
                           nconc (list arg " "))))))))

(defpsmacro create-element (node-type)
  `((@ document create-element) ,node-type))

(defpsmacro set-inner-html (el html)
  `(setf (slot-value ,el 'inner-h-t-m-l) ,html))

(defpsmacro remove-node (el)
  `((@ ,el parent-node remove-child) ,el))

(defparameter *pixel-styles*
  '(top bottom left right width height border-width))

;; FIXME might be faster to setAttributes
(defpsmacro set-style ((&rest var) &rest args)
  `(setf
    ,@(loop for (a b) on args by #'cddr
            nconc
            `((@ ,@var style ,a)
                       ,(if (and b (member a *pixel-styles*)) `(+ ,b "px") b)))))

(defun ensure-string (symbol-or-string)
  (etypecase symbol-or-string
    (symbol (symbol-name symbol-or-string))
    (string symbol-or-string)))

(defun string-starts-with (string prefix &key (test #'char=))
  "Returns true if STRING starts with PREFIX."
  (let ((prefix (ensure-string prefix))
        (string (ensure-string string)))
    (let ((mismatch (mismatch prefix string :test test)))
      (or (not mismatch) (= mismatch (length prefix))))))

(defun this-swap (from to)
  (cond
    ((eql from 'this) to)
    (t
     (let ((sfrom (symbol-name from))
           (sto (symbol-name to)))
       (and (string-starts-with sfrom "THIS.")
            (intern (concatenate 'string sto "." (subseq sfrom 5))))))))

(defun subthis (this tree)
  (labels ((s (subtree)
             (or (and (symbolp subtree) (this-swap subtree this))
                 (cond ((atom subtree) subtree)
                       (t (let ((car (s (car subtree)))
                                (cdr (s (cdr subtree))))
                            (if (and (eq car (car subtree))
                                     (eq cdr (cdr subtree)))
                              subtree
                              (cons car cdr))))))))
    (s tree)))

(defpsmacro defun-trace (name args &rest body)
  (let* ((sname (ps::symbol-to-js-string name))
         (tname (ps-gensym name))
         (this (ps-gensym "this"))
         (arg-names (loop for arg in args
                          unless (eq arg '&optional)
                            collect (if (consp arg) (car arg) arg)))
         (argpairs
          (loop for arg in arg-names
                nconc (list (ps::symbol-to-js-string arg) arg))))
    `(progn
       (defun ,tname (,this ,@args)
         ,@(subthis this body))
       (defun ,name ,arg-names
         (console *trace-level* ,sname ":" ,@argpairs)
         (incf *trace-level*)
         (let ((rtn (,tname this ,@arg-names)))
           (decf *trace-level*)
           (console *trace-level* ,sname "returned" rtn)
           (return rtn))))))

(defparameter *js-file*
  (ps*
   '(progn

     (defvar *trace-level* 0)

     (defvar *browser*
       (cond
         ((plusp ((@ (@ navigator user-agent) index-of) "Chrome")) :chrome)
         ((plusp ((@ (@ navigator user-agent) index-of) "Gecko")) :firefox)
         ((plusp ((@ (@ navigator user-agent) index-of) "MSIE")) :trident)
         (t :unknown)))

     (defun show (id)
       (with-id (o id)
         (setf (@ o style visibility) "visible")))

     (defun hide (id)
       (with-id (o id)
         (setf (@ o style visibility) "hidden")))

     (defun visit (id)
       (setf (@ window location) (+ "/?id=" id)))

     (defun word (id index)
       (setf (@ window location) (+ "/?id=" id "&index=" index)))

     (defun word-text (id index)
       (setf (@ window location) (+ "/?id=" id "&word=" index)))

     (defun go (url) (setf (@ window location) url))

     (defun get-by-id (id &optional (error t))
       (let ((hit ((@ document get-element-by-id) id)))
         (if hit
           (return hit)
           (if error (console "ERROR: get-by-id" id)))))

     (defvar *editor*)

     (defun add-new-child (element position name &optional attributes body)
       (let ((new-element (create-element name))
             (num (if attributes (/ (@ attributes length) 2) 0)))
         (dotimes (x num)
           (let ((name (aref attributes (* 2 x)))
                 (value (aref attributes (1+ (* 2 x)))))
             (set-att new-element name value)))
         (when body (set-inner-html new-element body))
         (cond
           ((and (= position "beginning") (@ element first-child))
            ((@ element insert-before) new-element (@ element first-child)))
           (t ((@ element append-child) new-element)))
         (return new-element)))

     (defun is-trident ()
       "Return the Trident version; NIL if the client is not Internet Explorer."
       (let ((foundp ((@ (@ navigator user-agent) search)
                      "MSIE ([0-9]{1,}[\.0-9]{0,})")))
         (when (> foundp -1)
           (return (parse-float (@ *reg-exp '$1))))
         (return nil)))

     (defun set-att (el name value)
       (let ((o (if (= (typeof el) "string") (get-by-id el) el)))
         (if value
           ;; IE's setAttribute doesn't work with `style' and `class'
           ;; In 8.0 they corrected the latter (obliterating the previous approach)
           ;; but kept the former.
           ;; TODO: there are some other attributes, see http://gist.github.com/266694
           (cond
             ((and (is-trident)
                   (= name "style"))
              (setf (@ (@ o style) 'css-text) value))
             ((and (is-trident) (< (is-trident) 8)
                   (= name "class"))
              ((@ o set-attribute) "className" value))
             (t
              ((@ o set-attribute) name value)))
           ((@ o remove-attribute) name))))

     (defun get-offset (el)
       (let ((left (parse-int (@ el offset-left)))
             (top (parse-int (@ el offset-top)))
             (scroll-left (@ el scroll-left))
             (scroll-top (@ el scroll-top)))
         ;; http://doiop.com/ie+offsetparent
         (try (setf el (@ el offset-parent)) (:catch (e)))
         (while (!== el null)
           (incf left (parse-int (@ el offset-left)))
           (incf top (parse-int (@ el offset-top)))
           (incf scroll-left (@ el scroll-left))
           (incf scroll-top (@ el scroll-top))
           (try (setf el (@ el offset-parent)) (:catch (e))))
         (return (create :left left :top top
                         :scrollleft scroll-left :scrolltop scroll-top))))

     (defun show-editor (event target-id left width height contents)
       (let* ((target (get-by-id target-id))
              (target-position (get-offset target)))
         (if *editor*
           (setf (@ *editor* style top) (@ target-position top))
           (setf *editor*
                 (add-new-child (get-by-id "body")
                                "beginning" "div"
                                (array :class "editor"
                                       :style (+ "position:absolute;"
                                                 "top:" (@ target-position top) ";"
                                                 "left:" left ";"
                                                 "float:right;")))))
         (set-inner-html *editor*
                         (+ "<table><tr><td>"
                            (+ "<textarea id=\"edta\" style=\"background-color:black;color:white;"
                               "padding:10px;border-radius:5px;"
                               "width:" width "px;height:" height "px;\" >"
                               contents
                               "</textarea>")
                            "</td></tr><tr><td>"
                            "<img id=\"edtab\" class=\"page-button\" src=\"/images/check.png\" />"
                            "</td></tr></table></div>"))
         (let ((textarea (get-by-id "edta"))
               (check (get-by-id "edtab")))
           (setf (@ check onclick)
                 (lambda (event)
                   (add-comment target (@ textarea value))
                   ((@ *editor* remove))
                   (setf *editor* nil)))
           (setf (@ textarea onchange)
                 (lambda (event)
                   (let ((text (@ textarea value)))
                     (add-comment target text t)
                     (setf (@ target draft) text)))))
         (when (@ target draft)
           (setf (@ textarea value) (@ target draft)))
         ((@ textarea focus))))

     (defun request (command &optional arguments)
       (unless arguments (setf arguments (create)))
       (setf (@ arguments :command) command)
       (http-request "/a" arguments))

     (defun add-comment (target text &optional draft)
       (request (if draft "add-comment-draft" "add-comment")
                (create :text text :target (@ target id))))

     (defun encode-arguments (arguments)
       (let ((arr (make-array)))
         (for-in (key arguments)
           (let ((val (aref arguments key)))
             (setf
              (aref arr (length arr))
              (+ (encode-u-r-i-component key) "=" (encode-u-r-i-component val)))))
         (return ((@ ((@ arr join) "&") replace) (regex "/%20/g") "+"))))

     (defvar *print-response* nil)

     (defun http-request (url arguments)
       (let* ((encoded-arguments (when arguments (encode-arguments arguments)))
              (req (new |:XMLHttpRequest|))
              (change-fn
                (lambda ()
                  (when (!= (@ req ready-state) 4) (return))
                  (unless (= (slot-value req 'status) 200)
                    (console "ERROR: ajax" req)
                    (return))
                  (set-timeout
                   (lambda ()
                     (let ((response (slot-value req 'response-text)))
                       (when *print-response* (console "response:" response))
                       (eval response)))
                   10))))
         ((@ req open) "GET" (if encoded-arguments (+ url "&" encoded-arguments) url) t)
         ((@ req set-request-header) "X-Requested-With" "XMLHttpRequest")
         (setf (@ req onreadystatechange) change-fn)
         ((@ req send) nil)))

     (defun show-settings ()
       (show "settings-icon"))

     (defun hide-settings ()
       (hide "settings-icon"))

     (defun show-settings-dialog ()
       (close-dialog t)
       (request "show-settings-dialog"))

     (defun show-avatars-dialog ()
       (close-dialog t)
       (request "show-avatars-dialog"))

     (defvar *backpane*)
     (defvar *dialog*)

     (defun show-dialog (html)
       (let ((body (get-by-id "body")))
         (unless *backpane*
           (setf *backpane*
                 (add-new-child body nil "div"
                                (array :class "dialog-backpane"))))
         (setf *dialog*
               (add-new-child body nil "div"
                              (array :class "dialog-outer")
                              (+ "<div class=\"dialog-inner\"><div class=\"dialog\">"
                                 html "</div></div>")))))

     (defun close-dialog (&optional nobackpane)
       (when *backpane*
         (unless nobackpane
           (remove-node *backpane*)
           (setf *backpane* nil))
         (remove-node *dialog*)
         (setf *dialog* nil)))

     (defun icon-src (icon)
       (return
         (+ "/images/" icon
            (if (= ((@ icon index-of) "v/") 0)
              ""
              ".png"))))

     (defun set-contents (id body)
       (set-inner-html (get-by-id id) body))

     (defun start-iscroll (id)
       (new (i-scroll id (create :h-scrollbar nil :v-scrollbar nil))))

     (defun set-class (id new-class)
       (let ((el (get-by-id id)))
         (setf (slot-value el 'class-name) new-class)))

     (defun timestamp ()
       (return (new ((@ (*date ) get-time)))))

     (defun reload-calibration-image ()
       (let ((el (get-by-id "calibration")))
         (setf (slot-value el 'src) (+ "/images/calibration.png?id=" (timestamp)))))

     (defun js-ensure-list (x) ; prevent name conflict with metatilities ensure-list
       (if (= (type-of x) "array")
         (return x)
         (return (array x))))

     (defun type-of (o)
       "Sane typeof operator."
       ;; http://javascript.crockford.com/remedial.html
       (let ((type (typeof o)))
         (cond
           ((and (=== type "object") o (instanceof o *array))
            (return "array"))
           ((=== type "object")
            (return nil))
           (t
            (return type)))))

     (defun set-unselectable (ids)
      (dolist (id (js-ensure-list ids))
        (let ((o (get-by-id id)))
          (when o
            (setf (@ o unselectable) "on"
                  (@ o onselectstart) (lambda () (return false)))
            (when (@ o style)
              (setf (@ o style *moz-user-select) "none")))))))))

(defun js-file () *js-file*)


